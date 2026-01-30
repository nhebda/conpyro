# Convert combinations of stand attributes to numeric codes
fn_mcsa_idx <- function(FFMC, season, density, stand, model_mcsa) {
  # Normalize input
  FFMC <- as.numeric(FFMC)
  season <- tolower(as.character(season))
  density <- tolower(as.character(density))
  stand <- tolower(as.character(stand))
  model_mcsa <- tolower(model_mcsa)
  # Validate input
  assert_number(FFMC, lower = 80, upper = 99)
  assert_choice(
    season,
    choices = c("spring","sp-su", "summer", "fall", "1", "1.5", "2", "3")
  )
  assert_choice(
    density,
    choices = c("light", "moderate", "dense", "1", "2", "3")
  )
  assert_choice(
    stand,
    choices = c(
      "deciduous",
      "douglas-fir",
      "mixedwood",
      "pine",
      "spruce",
      "d",
      "df",
      "m",
      "p",
      "s"
    )
  )
  assert_choice(
    model_mcsa,
    choices = c("original", "corrected")
  )
  # Map codes
  season_map <- c(
    "spring" = 1, "1" = 1,
    "summer" = 2, "2" = 2,
    "fall" = 3, "3" = 3,
    "sp-su" = 4, "1.5" = 4
  )
  density_map <- c(
    "light" = 1, "1" = 1,
    "moderate" = 2, "2" = 2,
    "dense" = 3, "3" = 3
  )
  stand_map <- c(
    "deciduous" = 1, "d"  = 1,
    "douglas-fir" = 2, "df" = 2,
    "mixedwood" = 3, "m"  = 3,
    "pine" = 4, "p" = 4,
    "spruce" = 5, "s" = 5
  )
  # Assign codes
  season_code  <- season_map[[season]]
  density_code <- density_map[[density]]
  stand_code   <- stand_map[[stand]]
  # Density adjustment
  density_adj <- density_code
  if (model_mcsa == "corrected") {
    if (density_code == 1 && FFMC > 96.15) density_adj <- 2
    if (density_code == 3 && FFMC > 92.93) density_adj <- 2
  }
  # Encode
  idx <- as.numeric(
    paste0(season_code, density_adj, stand_code)
  )
  return(idx)
}
