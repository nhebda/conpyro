## read-in raw data
input <- read.csv("inst/extdata/input_default.csv", header = TRUE)

## export the dataset
usethis::use_data(input, overwrite = TRUE, internal = FALSE)
