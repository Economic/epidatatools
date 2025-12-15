## code to prepare `state_geocodes` dataset

state_geocodes <- read.csv("data-raw/state_geocodes.csv")

state_geocodes <- dplyr::arrange(state_geocodes, state_name)

usethis::use_data(state_geocodes, overwrite = TRUE)
