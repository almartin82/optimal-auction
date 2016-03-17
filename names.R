library(magrittr)
library(dplyr)

## NAME DISPLAY STRING
proj1$h_final$disp_name <- paste0(
  proj1$h_final$fullname, ' (', proj1$h_final$priority_pos, ')'
)

proj1$p_final$disp_name <- paste0(
  proj1$p_final$fullname, ' (', proj1$p_final$priority_pos, ')'
)

player_disp_name <- c(proj1$h_final$disp_name, proj1$p_final$disp_name)


match_fields <- . %>%
  dplyr::select(
    disp_name, mlbid
  )

h_fields <- match_fields(proj1$h_final)
p_fields <- match_fields(proj1$p_final)
match_df <- rbind(h_fields, p_fields)


