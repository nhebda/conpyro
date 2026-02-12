#' Centralized input validation function
#'
#' @keywords internal
#' @importFrom checkmate assert_number assert_numeric assert_choice
#'   assert_integerish
#'
fn_validate_input <- function(...) {
  input <- list(...)
  coll <- makeAssertCollection()
  # List validation rules as functions
  rules <- list(
    "FFMC" = function(name, coll) {
      assert_number(
        name,
        lower = 80,
        upper = 99,
        finite = TRUE,
        .var.name = "FFMC",
        add = coll
      )
    },
    "DMC" = function(name, coll) {
      assert_number(
        name,
        lower = 5,
        upper = 250,
        finite = TRUE,
        .var.name = "DMC",
        add = coll
      )
    },
    "WS10" = function(name, coll) {
      assert_numeric(
        name,
        lower = 0,
        upper = 60,
        finite = TRUE,
        min.len = 1,
        .var.name = "WS10",
        add = coll
      )
    },
    "mc" = function(name, coll) {
      assert_number(
        name,
        lower = 0,
        upper = 80,
        finite = TRUE,
        .var.name = "mc",
        add = coll
      )
    },
    "season" = function(name, coll) {
      assert_choice(
        tolower(as.character(name)),
        choices = c("spring","sp-su", "summer", "fall", "1", "1.5", "2", "3"),
        .var.name = "season",
        add = coll
      )
    },
    "density" = function(name, coll) {
      assert_choice(
        tolower(as.character(name)),
        choices = c("light", "moderate", "dense", "1", "2", "3"),
        .var.name = "density",
        add = coll
      )
    },
    "stand" = function(name, coll) {
      assert_choice(
        tolower(name),
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
        ),
        .var.name = "stand",
        add = coll
      )
    },
    "model_mcsa" = function(name, coll) {
      assert_choice(
        tolower(name),
        choices = c("original", "corrected"),
        .var.name = "model_mcsa",
        add = coll
      )
    },
    "model_pCFO_mcF" = function(name, coll) {
      assert_choice(
        name,
        choices = c(7L, 10L),
        .var.name = "model_pCFO",
        add = coll
      )
    },
    "model_pCFO_mcF" = function(name, coll) {
      assert_choice(
        name,
        choices = c(8L, 11L),
        .var.name = "model_pCFO",
        add = coll
      )
    },
    "FSG" = function(name, coll) {
      assert_number(
        name,
        lower = 0.5,
        upper = 20,
        finite = TRUE,
        .var.name = "FSG",
        add = coll
      )
    },
    "SFC" = function(name, coll) {
      assert_number(
        name,
        lower = 0.1,
        upper = 6,
        finite = TRUE,
        .var.name = "SFC",
        add = coll
      )
    },
    "month" = function(name, coll) {
      assert_integerish(
        name,
        len = 1L,
        .var.name = "month",
        add = coll
      )
    },
    "day" = function(name, coll) {
      assert_integerish(
        name,
        len = 1L,
        .var.name = "day",
        add = coll
      )
    },
    "canopy_closure" = function(name, coll) {
      assert_number(
        name,
        lower = 20,
        upper = 100,
        .var.name = "canopy_closure",
        add = coll
      )
    }
  )
  # Check assertions by calling relevant rules functions and add to collection
  input_names <- names(input)
  rules_names <- names(rules)
  matched_names <- intersect(input_names, rules_names)
  unmatched_names <- setdiff(input_names, rules_names)
  invisible(lapply(matched_names, function(name) {
    rules[[name]](input[[name]], coll)
  }))
  if (length(unmatched_names) > 0) {
    warning(
      "The following elements have no validation rules: ",
      paste(unmatched_names, collapse = ", ")
    )
  }
  tryCatch(
    reportAssertions(coll),
    error = function(e) {
      # Strip the 'Error in ...' prefix by using call. = FALSE
      stop(conditionMessage(e), call. = FALSE)
    }
  )
}
