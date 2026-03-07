#' Centralized input validation function
#'
#' @keywords internal
#' @importFrom checkmate assert assert_character assert_choice assert_integerish
#'   assert_logical assert_number assert_numeric assert_subset check_character
#'   check_numeric makeAssertCollection reportAssertions
#'
validate_input <- function(...) {
  input <- list(...)
  coll <- makeAssertCollection()
  # List validation rules as functions
  rules <- list(
    # Required
    "WS10" = function(name, coll) {
      assert_numeric(
        name,
        lower = 0,
        upper = 60,
        min.len = 1L,
        .var.name = "WS10",
        add = coll
      )
    },
    "FFMC" = function(name, coll) {
      assert_numeric(
        name,
        lower = 80,
        upper = 99,
        any.missing = FALSE,
        min.len = 1L,
        .var.name = "FFMC",
        add = coll
      )
    },
    "FSG" = function(name, coll) {
      assert_numeric(
        name,
        lower = 0.5,
        upper = 20,
        any.missing = FALSE,
        min.len = 1L,
        .var.name = "FSG",
        add = coll
      )
    },
    "SFC" = function(name, coll) {
      assert_numeric(
        name,
        lower = 0.1,
        upper = 6,
        any.missing = FALSE,
        min.len = 1L,
        .var.name = "SFC",
        add = coll
      )
    },
    "CBD" = function(name, coll) {
      assert_numeric(
        name,
        lower = 0.01,
        upper = 0.8,
        any.missing = FALSE,
        min.len = 1L,
        .var.name = "CBD",
        add = coll
      )
    },

    # Optional, no defaults
    "DMC" = function(name, coll) {
      assert_numeric(
        name,
        lower = 5,
        upper = 250,
        .var.name = "DMC",
        add = coll
      )
    },
    "season" = function(name, coll) {
      assert_subset(
        tolower(as.character(name)),
        choices = c(
          "spring",
          "sp-su",
          "summer",
          "fall",
          "1",
          "1.5",
          "2",
          "3",
          NA
        ),
        .var.name = "season",
        add = coll
      )
    },
    "density" = function(name, coll) {
      assert_subset(
        tolower(as.character(name)),
        choices = c("light", "moderate", "dense", "1", "2", "3", NA),
        .var.name = "density",
        add = coll
      )
    },
    "stand" = function(name, coll) {
      assert_subset(
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
          "s",
          NA
        ),
        .var.name = "stand",
        add = coll
      )
    },

    # Optional, with defaults
    "smooth_CFO" = function(name, coll) {
      assert_logical(
        name,
        null.ok = TRUE,
        .var.name = "smooth_CFO",
        add = coll
      )
    },
    "ID" = function(name, coll) {
      assert(
        check_character(
          name,
          any.missing = FALSE,
          unique = TRUE
        ),
        check_numeric(
          name,
          any.missing = FALSE,
          unique = TRUE
        ),
        combine = "or",
        .var.name = "ID",
        add = coll
      )
    },
    "CF_thresh" = function(name, coll) {
      assert_numeric(
        name,
        lower = 0,
        upper = 1,
        null.ok = TRUE,
        .var.name = "CF_thresh",
        add = coll
      )
    },
    "plot" = function(name, coll) {
      assert_subset(
        tolower(name),
        choices = c("pcfo", "cac", "ros"),
        .var.name = "plot",
        add = coll
      )
    },
    "model_mcsa" = function(name, coll) {
      assert_subset(
        tolower(name),
        choices = c("original", "corrected", NA),
        .var.name = "model_mcsa",
        add = coll
      )
    },
    "model_pCFO" = function(name, coll) {
      assert_subset(
        name,
        choices = c(7L, 8L, 10L, 11L, NA),
        .var.name = "model_pCFO",
        add = coll
      )
    },
    "model_sROS" = function(name, coll) {
      assert_subset(
        name,
        choices = c(1L, 2L, 3L, 4L, 12L, 13L, NA),
        .var.name = "model_sROS",
        add = coll
      )
    },
    "model_cROS" = function(name, coll) {
      assert_subset(
        name,
        choices = c(1L, 2L, 3L, NA),
        .var.name = "model_cROS",
        add = coll
      )
    },

    # Tools-specific inputs
    "month" = function(name, coll) {
      assert_integerish(
        as.numeric(name),
        len = 1L,
        .var.name = "month",
        add = coll
      )
    },
    "day" = function(name, coll) {
      assert_integerish(
        as.numeric(name),
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
    },
    "mc" = function(name, coll) {
      assert_number(
        name,
        lower = 1,
        upper = 30,
        .var.name = "mc",
        add = coll
      )
    },
    "LAT" = function(name, coll) {
      assert_number(
        name,
        lower = 41,
        upper = 70,
        .var.name = "LAT",
        add = coll
      )
    },
    "LONG" = function(name, coll) {
      assert_number(
        name,
        lower = 52,
        upper = 141,
        .var.name = "LONG",
        add = coll
      )
    },
    "ELV" = function(name, coll) {
      assert_number(
        name,
        lower = 0,
        upper = 2500,
        na.ok = TRUE,
        .var.name = "ELV",
        add = coll
      )
    },
    "Dj" = function(name, coll) {
      assert_integerish(
        name,
        lower = 1,
        upper = 366,
        .var.name = "Dj",
        add = coll
      )
    },
    "BUI" = function(name, coll) {
      assert_number(
        name,
        lower = 5,
        upper = 200,
        .var.name = "BUI",
        add = coll
      )
    },
    "PC" = function(name, coll) {
      assert_number(
        name,
        lower = 0,
        upper = 100,
        .var.name = "PC",
        add = coll
      )
    },
    "FFL" = function(name, coll) {
      assert_number(
        name,
        lower = 1,
        upper = 5,
        .var.name = "FFL",
        add = coll
      )
    },
    "FWFL" = function(name, coll) {
      assert_number(
        name,
        lower = 0,
        upper = 2,
        .var.name = "FWFL",
        add = coll
      )
    },

    # Ladder fuels inputs
    "cons" = function(name, coll) {
      assert_number(
        name,
        lower = 0.1,
        upper = 10,
        .var.name = "cons",
        add = coll
      )
    },
    "cl" = function(name, coll) {
      assert_number(
        name,
        lower = 0.5,
        upper = 15,
        .var.name = "cl",
        add = coll
      )
    },
    "hs" = function(name, coll) {
      assert_number(
        name,
        lower = 0.5,
        upper = 10,
        .var.name = "hs",
        add = coll
      )
    },
    "zs" = function(name, coll) {
      assert_number(
        name,
        lower = 0.5,
        upper = 9,
        .var.name = "zs",
        add = coll
      )
    },
    "zp" = function(name, coll) {
      assert_number(
        name,
        lower = 0.5,
        upper = 20,
        .var.name = "zp",
        add = coll
      )
    },
    "lnfl" = function(name, coll) {
      assert_number(
        name,
        lower = 0.1,
        upper = 2,
        .var.name = "lnfl",
        add = coll
      )
    },
    "sapling_FMC" = function(name, coll) {
      assert_number(
        name,
        lower = 80,
        upper = 120,
        .var.name = "sapling_FMC",
        add = coll
      )
    },
    "actual_SFC" = function(name, coll) {
      assert_number(
        name,
        lower = 0.1,
        upper = 6,
        .var.name = "actual_SFC",
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

  # Diagnostic use only; remove from final version
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
