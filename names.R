devtools::load_all(pkg = file.path('..', 'projprep'))

library(magrittr)
library(dplyr)

## NAME DISPLAY STRING
match_df <- projprep::prep_name_metadata(id_map)
player_disp_name <- match_df$disp_name

