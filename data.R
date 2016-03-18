library(ggplot2)

proj_files <- list.files(path = proj_data_path)
num_projs <- length(proj_files)

all_proj <- list()
#load the projections in
for (i in seq_along(proj_files)) {
  proj_name <- gsub('.rds', '', proj_files[i])
  file_loc <- file.path(proj_data_path, proj_files[i])
  data <- readRDS(file_loc)
  #put the projprep data into memory
  assign(paste("proj", i, sep = ""), data)
  #put it on the big list
  all_proj[[proj_name]] <- data
}


devtools::load_all(pkg = file.path('..', 'projprep'))

stat_dist <- function(pp_list, stat, playerid, player_pos) {
  this_stat <- lapply(
    pp_list, stat_extract_all, hit_pitch = 'h', stat = stat
  )
  this_stat_pos <- lapply(
    pp_list, stat_extract_pos, hit_pitch = 'h', stat = stat
  )
  this_stat_player <- lapply(
    pp_list, stat_extract_player, hit_pitch = 'h', 
    stat = stat, playerid = playerid
  )

  full_stat_list <- lapply(seq_along(this_stat), function(i) {
    data.frame(
      stat = this_stat[[i]],
      system = names(this_stat)[[i]],
      stringsAsFactors = FALSE
    )
  })
  full_df <- dplyr::bind_rows(full_stat_list)
  
  agg_stat_list <- lapply(seq_along(this_stat_pos), function(i) {
    int_df <- data.frame(
      pos = this_stat_pos[[i]][, 1],
      stat = this_stat_pos[[i]][, 2],
      system = names(this_stat_pos)[[i]],
      stringsAsFactors = FALSE
    )
    names(int_df) <- c('pos', 'stat', 'system')
    
    int_df %>%
      dplyr::group_by(pos, system) %>%
      dplyr::summarise(
        q_25 = quantile(stat, probs = 0.25),
        q_50 = quantile(stat, probs = 0.50),
        q_75 = quantile(stat, probs = 0.75)
      )
  })
  agg_df <- dplyr::bind_rows(agg_stat_list) %>%
    dplyr::filter(
      pos == player_pos
    )
  
  player_stat_list <- lapply(seq_along(this_stat_player), function(i) {
    data.frame(
      stat = this_stat_player[[i]],
      #to handle missing data
      system = rep(names(this_stat_player)[[i]], length(this_stat_player[[i]])),
      stringsAsFactors = FALSE
    )
  })
  player_df <- dplyr::bind_rows(player_stat_list)
  
  print(player_df)
  
  ggplot() +
  geom_violin(
    data = full_df,
    aes(
      x = system,
      y = stat
    ),
    scale = 'count', 
    fill = 'lightblue', 
    alpha = 0.25,
    color = 'white'
  ) +
  geom_jitter(
    data = full_df,
    aes(
      x = system,
      y = stat
    ),
    width = 0.0,
    height = 0.5,
    alpha = 0.1,
    shape = 3
  ) +
  geom_text(
    data = agg_df,
    aes(
      x = system,
      y = q_25,
      label = '25'
    ),
    alpha = 0.5,
    size = 3,
    vjust = -1
  ) +
  geom_text(
    data = agg_df,
    aes(
      x = system,
      y = q_50,
      label = '50'
    ),
    alpha = 0.5,
    size = 3,
    vjust = -1
  ) +   
  geom_text(
    data = agg_df,
    aes(
      x = system,
      y = q_75,
      label = '75'
    ),
    alpha = 0.5,
    size = 3,
    vjust = -1
  ) +
  geom_point(
    data = player_df,
    aes(
      x = system,
      y = stat
    ),
    color = 'hotpink'
  ) +
  theme_bw() +
  labs(
    y = stat
  ) +
  coord_flip()
}

stat_dist(all_proj, 'r', 545361, 'OF')


