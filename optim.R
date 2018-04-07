#' Optimize Auction (hitters)
#'
#' @param projprep a projprep object
#' @param roster_positions vector of roster positions to optimize for
#' @param stat_names vector of projected stat names to consider 
#' @param universe_depth how many players should we consider in the
#' projection data?  default is top 200.
#'
#' @return an optauction object
#' @export

optimize_h_auction <- function(
  projprep, 
  expected_prices = adp_expected_prices(),
  roster_positions = c('C', '1B', '2B', 'SS', '3B', 'OF'), 
  stat_names =  c('r', 'rbi', 'sb', 'tb', 'obp'),
  universe_depth = 200
) {
  #get the player universe
  player_univ <- projprep %>% 
    extract2('h_final') %>%
    ungroup() %>%
    top_n(universe_depth, wt = final_zsum) 
  num_players <- nrow(player_univ)

  #multiposition players need special constraints
  multipos_players <- player_univ %>%
    filter(grepl(',', position, fixed = TRUE))
  
  #lists that will be used in the settings of constraints
  player_only <- list()
  pos_only <- list()
  player_pos <- list()
  player_pos_stat <- list()
  
  for (i in 1:nrow(player_univ)) {
    this_row <- player_univ[i, ]
    this_pos <- strsplit(this_row$position, ', ')[[1]]
    this_player_pos <- paste(this_row$mlbid, this_pos, sep = '_')
    this_player_pos_stat <- map(
      this_player_pos, 
      function(.x) paste(.x, stat_names, sep = '_')
    ) %>% reduce(c)
    
    player_only[[i]] <- rep(this_row$mlbid, length(this_player_pos_stat))
    pos_only[[i]] <- gsub(
      "(\\d+)([[:punct:]]+)([[:alnum:]]+)(.+)", 
      "\\3", 
      this_player_pos_stat
    )
    player_pos[[i]] <- gsub(
      "(\\d+)([[:punct:]]+)([[:alnum:]]+)(.+)", 
      "\\1\\2\\3", 
      this_player_pos_stat
    )
    player_pos_stat[[i]] <- this_player_pos_stat
  }
  
  player_only <- player_only %>% reduce(c)
  pos_only <- pos_only %>% reduce(c)
  player_pos <- player_pos %>% reduce(c)
  player_pos_stat <- player_pos_stat %>% reduce(c)
  unq_player_pos <- player_pos %>% unique()

  #names of dummy discontinuous variables
  p_discon_dummy <- paste0(unq_player_pos, '_play_cstrt')
  p_multipos_dummy <- paste0(multipos_players$mlbid, '_multipos_cstrt')
  b_player_dummy <- paste0(player_univ$mlbid, '_binary_player_cstrt')
  
  #expected_price <- paste0(player_univ$mlbid, '_exp_price')
  auction_price <- paste0(player_univ$mlbid, '_auction_price')
  b_price_dummy <- paste0(player_univ$mlbid, '_binary_auction_cstrt')

  metadata <- tibble(
    final_vars = c(
      #the real decision variables
      player_pos, 
      #shadow player variables enforcing constraints
      p_discon_dummy, p_multipos_dummy, b_player_dummy, 
      #price
      auction_price,
      #shadow price contraints
      b_price_dummy
    ),
    var_type = c(
      rep('player_position', length(player_pos)),
      rep('player_pos_discon_dummy', length(p_discon_dummy)),
      rep('player_multipos_discon_dummy', length(p_multipos_dummy)),
      rep('binary_player_dummy', length(b_player_dummy)),
      rep('auction_price', length(auction_price)),
      rep('binary_price_dummy', length(b_price_dummy))
    )
  ) %>%
  mutate(
    mlbid = str_sub(final_vars, 1, 6)
  )
  metadata$row_number <- row.names(metadata) %>% as.integer()
  
  
  final_vars <- metadata$final_vars
  
  #initialize the lp model
  best <- make.lp(0,  length(final_vars), verbose = 'normal')
  
  #player constraints
  for (i in seq(1:length(unq_player_pos))) {
    this_id <- gsub("(\\d+)([[:punct:]]+)([[:alnum:]]+)", "\\1", unq_player_pos[i])
    this_player_pos <- unq_player_pos[i]
    player_df <- player_univ[player_univ$mlbid == this_id, ]
    this_player <- player_df$fullname
    
    #find player indices
    player_indices <- which(player_pos %in% this_player_pos)
    #find player dummy
    p_dummy_index <- which(final_vars == paste0(this_player_pos, '_play_cstrt'))
    
    add.constraint(
      lprec = best, 
      xt = c(rep(1, length(player_indices)), -1),
      type = "=",
      rhs = 0,
      indices = c(player_indices, p_dummy_index)
    ) 
  }
  
  #multipos player constraints
  if (nrow(multipos_players) >= 1) {
    for (i in 1:nrow(multipos_players)) {
      player_df <- multipos_players[i, ]
      
      #find player indices
      player_indices <- which(player_only %in% player_df$mlbid)
      #find player discontinuity dummy
      p_dummy_index <- which(final_vars == paste0(player_df$mlbid, '_multipos_cstrt'))
      
      add.constraint(
        lprec = best, 
        xt = c(rep(1, length(player_indices)), -1),
        type = "=",
        rhs = 0,
        indices = c(player_indices, p_dummy_index)
      ) 
    }
  }
  
  #position constraints
  for (i in 1:length(roster_positions)) {
    this_roster <- roster_positions[i]
    pos_indices <- which(pos_only %in% this_roster)
    
    add.constraint(
      lprec = best, 
      xt = rep(1, length(pos_indices)),
      type = "=",
      rhs = 5,
      indices = pos_indices
    )
  }
  
  #expected price constraints
  
  #all prices must be less than 270
  add.constraint(
    lprec = best,
    xt = rep(1, length(auction_price)),
    type = "<=",
    rhs = 270,
    indices = which(final_vars %in% auction_price)
  )
  
  #any player who isn't drafted should have zero price
  
  #first create a binary drafted/undrafted
  for (i in 1:length(b_player_dummy)) {
    this_mlbid <- gsub(
      "(\\d+)([[:punct:]]+)(.+)",
      "\\1",
      b_player_dummy[i]
    )
    
    #indices
    p_dummy_indices <- metadata %>% 
      filter(mlbid == this_mlbid & 
             var_type == 'player_pos_discon_dummy'
      ) %>%
      pull(row_number)
    b_dummy_index <- which(final_vars %in% b_player_dummy[i])
    
    add.constraint(
      lprec = best,
      xt = c(rep(-.2, length(p_dummy_indices)), 1),
      type = "=",
      rhs = 0,
      indices = c(p_dummy_indices, b_dummy_index)
    )
  }
  
  #then create a binary for auction greater than zero
  for (i in 1:length(b_price_dummy)) {
    this_mlbid <- gsub(
      "(\\d+)([[:punct:]]+)(.+)",
      "\\1",
      b_price_dummy[i]
    )
    p_dummy_indices <- metadata %>% 
      filter(mlbid == this_mlbid & 
               var_type == 'player_pos_discon_dummy'
      ) %>%
      pull(row_number)
    price_index <- metadata %>% 
      filter(mlbid == this_mlbid & 
               var_type == 'auction_price'
      ) %>%
      pull(row_number)
    b_dummy_index <- which(final_vars %in% b_price_dummy[i])
    
    add.constraint(
      lprec = best,
      xt = c(rep(-.2, length(p_dummy_indices)), -1, 1),
      type = "=",
      rhs = 0,
      indices = c(p_dummy_indices, price_index, b_dummy_index)
    )
  }
  
  #two binary dummies must be equal
  for (i in 1:length(b_price_dummy)) {
    this_mlbid <- gsub(
      "(\\d+)([[:punct:]]+)(.+)",
      "\\1",
      b_price_dummy[i]
    )
    
    binary_player_index <- which(
      final_vars %in% paste0(this_mlbid, '_binary_player_cstrt')
    )
    binary_price_index <- which(final_vars %in% b_price_dummy[i])
    
    add.constraint(
      lprec = best,
      xt = c(1, -1),
      type = "=",
      rhs = 0,
      indices = c(binary_player_index, binary_price_index)
    )
  }  
  
  #get data for objective function
  player_stat_zscore <- function(playerid, stat) {
    df <- player_univ %>%
      filter(mlbid == playerid)
    df %>% extract2(paste0(stat, '_zscore'))
  }
  
  player_stat_value <- map(
    player_pos_stat,
    function(.x) {
      player <- gsub("(\\d+)([[:punct:]]+)([[:alnum:]]+)(.+)", "\\1", .x)
      stat <- gsub("(\\d+)([[:punct:]]+)([[:alnum:]]+)([[:punct:]]+)(.+)", "\\5", .x)
      player_stat_zscore(player, stat)
    }
  ) %>%
  reduce(c) %>%
  round(3)
  
  #create vector for objective function
  final_obj <- c(
    player_stat_value,
    #zeros for all the various dummy constraints
    rep(0, length(final_vars) - length(player_stat_value))
  )
  set.objfn(best, final_obj)
  
  #discontinuous dummy vars to force player stats to move as group
  all_discon_indices <- which(grepl('_play_cstrt|_multipos_cstrt', final_vars))
  set.semicont(best, col = all_discon_indices)
  set.bounds(
    best, 
    lower = rep(4.9, length(all_discon_indices)),
    col = all_discon_indices
  )
  
  #discont constraints on price vars to force zero value for
  #undrafted players
  price_discon_indices <- which(final_vars %in% auction_price)
  set.semicont(best, col = price_discon_indices)
  set.bounds(
    best, 
    lower = rep(4.9, length(price_discon_indices)),
    col = price_discon_indices
  )
  
  #binary player_pos
  set.type(
    best, 
    col = seq(1:length(player_stat_value)), 
    type = "binary"
  ) 
  set.type(
    best,
    col = metadata %>% 
      filter(var_type %in% c('binary_player_dummy', 'binary_price_dummy')) %>%
      pull(row_number),
    type = "binary"
  )
  lp.control(best, sense="max")
  
  #solve
  write.lp(best, "best.lp", "lp") 
  solve(best)
  
  #extract solution and make meaning of the decision variables
  total_value <- get.objective(best)
  decision_vars <- get.variables(best)
  constraint_vars <- get.constraints(best)
  optimal <- decision_vars[1:length(player_pos_stat)]
  prices <- decision_vars[which(final_vars %in% auction_price)]
  
  player_dummies <- decision_vars[which(final_vars %in% player_pos)]
  p_discon_dummies <- decision_vars[which(final_vars %in% p_discon_dummy)]
  
  b_player_dummies <- decision_vars[which(final_vars %in% b_player_dummy)]
  price_vars <- decision_vars[which(final_vars %in% auction_price)]
  b_price_dummies <- decision_vars[which(final_vars %in% b_price_dummy)]
  
  optimal_players <- player_pos_stat[optimal == 1]
  optimal_players <- gsub(
    "(\\d+)([[:punct:]]+)([[:alnum:]]+)(.+)", 
    "\\1\\2\\3", 
    optimal_players
  ) %>% unique()
  
  optimal_mlbids <- map_dbl(
    optimal_players, 
    function(.x) strsplit(.x, '_')[[1]][[1]] %>% as.numeric
  )

  optimal_positions <- map_chr(
    optimal_players,
    function(.x) strsplit(.x, '_')[[1]][[2]]
  )

  player_stats <- player_univ %>%
    select(mlbid, fullname, ab, r, rbi, sb, tb, obp, value)
    
  optimal <- tibble(
    mlbid = optimal_mlbids,
    starting_pos = optimal_positions
  ) %>%
  inner_join(player_stats, by = 'mlbid')
  
  list(
    total_z = total_value,
    optimal_player_df = optimal,
    prices = prices,
    constraints = constraint_vars,
    decision_vars = decision_vars,
    list(
      player = player_dummies,
      player_discon = p_discon_dummies,
      binary_player = b_player_dummies,
      binary_prices = b_price_dummies,
      prices = price_vars
    )
  )
}


adp_expected_prices <- function() {
  #adp_data <- projprep::espn_adp()
  
  foo <- all_proj$steam$h_final %>% 
    select(mlbid, value) %>%
    mutate(
      value = ifelse(value <= 1, 1, round(value))
    ) %>%
    rename(
      expected_price = value
    )
  
  foo
}