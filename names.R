devtools::load_all(pkg = file.path('..', 'projprep'))

library(magrittr)
library(dplyr)

## NAME DISPLAY STRING
match_df <- id_map %>%
  dplyr::filter(!is.na(mlbid))

match_df$disp_name <- paste0(match_df$playername, ' (', match_df$pos, ')')

match_df <- match_df %>%
  dplyr::select(disp_name, mlbid)

#for autocomplete
player_disp_name <- match_df$disp_name