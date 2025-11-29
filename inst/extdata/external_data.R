## read-in raw data
default_input <- read.csv("inst/extdata/default_input.csv", header = TRUE)

## export the dataset
usethis::use_data(default_input, overwrite = TRUE, internal = FALSE)
