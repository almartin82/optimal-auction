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


#devtools::load_all(pkg = file.path('..', 'projprep'))
#library(devtools)
#devtools::install_github('almartin82/projprep')
library(projprep)

perc_rank <- function(x, xo)  length(x[x <= xo])/length(x)*100

stat_dist_all <- function(pp_list, playerid, player_pos, hit_pitch = 'h') {
  if (hit_pitch == 'h') {
    h_stats <- c('r', 'rbi', 'sb', 'tb', 'obp')
    this_stats_all <- lapply(
      all_proj, stat_extract_many, hit_pitch = 'h', 
      stat = h_stats
    )
    this_stat_player <- lapply(
      pp_list, stat_extract_player, hit_pitch = 'h', 
      stat = h_stats, playerid = playerid
    )
  } else if (hit_pitch == 'p') {
    p_stats <- c('w', 'sv', 'k', 'era', 'whip')
    this_stats_all <- lapply(
      all_proj, stat_extract_many, hit_pitch = 'p', 
      stat = p_stats
    )
    this_stat_player <- lapply(
      pp_list, stat_extract_player, hit_pitch = 'p', 
      stat = p_stats, playerid = playerid
    )
  }
  
  full_stat_list <- lapply(seq_along(this_stats_all), function(i) {
    int_df <- data.frame(
      system = names(this_stats_all)[[i]],
      stat1 = this_stats_all[[i]][,1],
      stat2 = this_stats_all[[i]][,2],
      stat3 = this_stats_all[[i]][,3],
      stat4 = this_stats_all[[i]][,4],
      stat5 = this_stats_all[[i]][,5],
      stringsAsFactors = FALSE
    )
    out <- gather(int_df, stat, value, -system)
    out
  })
  full_df <- dplyr::bind_rows(full_stat_list)
  
  player_stat_list <- lapply(seq_along(this_stat_player), function(i) {
    int_df <- data.frame(
      system = rep(
        names(this_stat_player)[[i]], nrow(this_stat_player[[i]])),
      stat1 = this_stat_player[[i]][,1],
      stat2 = this_stat_player[[i]][,2],
      stat3 = this_stat_player[[i]][,3],
      stat4 = this_stat_player[[i]][,4],
      stat5 = this_stat_player[[i]][,5],
      stringsAsFactors = FALSE
    )
    out <- gather(int_df, stat, value, -system)
    out
  })
  player_stats_df <- dplyr::bind_rows(player_stat_list)


  out <- ggplot() +
  geom_violin(
    data = full_df,
    aes(
      x = system,
      y = value
    ),
    scale = 'count', 
    fill = 'lightblue', 
    alpha = 0.25,
    color = 'white'
  ) +
  geom_point(
    data = player_stats_df,
    aes(
      x = system,
      y = value
    ),
    color = 'hotpink'
  ) +
  theme_bw() +
  facet_grid(stat ~ ., scales = "free") +
  theme(
    panel.grid = element_blank()
  ) 
    
  out
}
#stat_dist_all(all_proj, 477132, 'SP')
#stat_dist_all(all_proj, 545361, 'OF')

price_table <- function(pp_list, playerid, hit_pitch) {
  this_stat_player <- lapply(
    pp_list, stat_extract_player, hit_pitch = hit_pitch, 
    stat = 'value', playerid = playerid
  )
  
  player_stat_list <- lapply(seq_along(this_stat_player), function(i) {
    int_df <- data.frame(
      system = rep(
        names(this_stat_player)[[i]], nrow(this_stat_player[[i]])),
      value = this_stat_player[[i]][,1],
      stringsAsFactors = FALSE
    )
    int_df
  })
  player_stats_df <- dplyr::bind_rows(player_stat_list)
  
  ggplot(
    data = player_stats_df,
    aes(
      x = 1,
      y = 1,
      label= round(value, 0)
    )
  ) +
  geom_text(
    size = 20
  ) +
  facet_grid(. ~ system) +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    title = element_blank()
  )
}
#price_table(all_proj, 477132, 'p')
#price_table(all_proj, 545361, 'h')

