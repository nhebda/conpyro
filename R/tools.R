#' pCFO calculator
#'
#' @param model
#' @param mc
#' @param ws
#' @param fsg
#' @param sfc
#'
#' @returns
#' @export
#'
#' @examples {
#' ## TODO
#' }
#' @importFrom checkmate assert_choice
tool_pcfo <- function(model = 11, mc = 7.8, ws = 20, fsg = 9.5, sfc = 1.8) {
  assert_choice(model, c(7, 8, 10, 11))
  assert_number(mc, lower = 5, upper = 20)
  assert_number(ws, lower = 0, upper = 60)
  assert_number(fsg, lower = 0.5, upper = 20)
  assert_number(sfc, lower = 0.1, upper = 6)
  pcfo <- fn_pcfo(model, ws, fsg, sfc, mc)
  out  <- list("pcfo" = pcfo)
  out
}

#' MC calculator
#'
#' @param ffmc
#' @param dmc
#' @param season
#' @param density
#' @param stand
#'
#' @returns
#' @export
#'
#' @examples
#'
tool_mc <- function(
    ffmc    = 91.2,
    dmc     = 75,
    season  = "summer",
    density = "moderate",
    stand   = "pine"
) {
  assert_number(ffmc, lower = 80, upper = 99)
  assert_number(dmc, lower = 5, upper = 200)
  assert_choice(
    tolower(season),
    c("spring","sp-su", "summer", "fall"),
    .var.name = "season"
  )
  assert_choice(
    tolower(density),
    c("light", "moderate", "dense"),
    .var.name = "density"
  )
  assert_choice(
    tolower(stand),
    c("deciduous", "douglas-fir", "mixedwood", "pine", "spruce"),
    .var.name = "stand"
  )
  mcffmc <- fn_mcffmc(ffmc)
  mcdmc  <- fn_mcdmc(dmc)
  idx    <- fn_mcsa_idx(tolower(season), tolower(density), tolower(stand))
  mcsa   <- fn_mcsa(idx, mcffmc, mcdmc)
  out    <- list("mcffmc" = mcffmc, "mcsa" = mcsa)
  out
}

#' FMC calculator
#'
#' @param lat
#' @param long
#' @param elv
#' @param dj
#'
#' @returns
#' @export
#'
#' @examples
#'
#' @importFrom cffdrs foliar_moisture_content
tool_fmc <- function(lat = 48, long = 83.3, elv = 100, dj = 200) {
  assert_number(lat, lower = 42, upper = 70)
  assert_number(long, lower = 53, upper = 141)
  assert_number(elv, lower = 0, upper = 2500)
  assert_number(dj, lower = 0, upper = 365)
  fmc <- foliar_moisture_content(lat, long, elv, dj, 0)
  out <- list("fmc" = fmc)
  out
}

#' FBP SFC calculator
#'
#' @param bui
#' @param ffmc
#' @param pc
#'
#' @returns
#' @export
#'
#' @examples
#'
#' @importFrom cffdrs surface_fuel_consumption
tool_sfc_fbp <- function(
    bui  = 85,
    ffmc = 91,
    pc   = 40
) {
  assert_number(bui, lower = 0, upper = 200)
  assert_number(ffmc, lower = 80, upper = 99)
  assert_number(pc, lower = 0, upper = 100)
  fueltypes <- c("C1", "C2", "C3", "C5", "C7", "D1", "M1", "S1", "S2", "S3")
  out <- lapply(
    fueltypes,
    surface_fuel_consumption,
    BUI  = bui,
    FFMC = ffmc,
    PC   = pc
  )
  names(out) <- c(
    "C1",
    "C2/M3/M4",
    "C3/C4",
    "C5/C6",
    "C7",
    "D1/D2",
    "M1/M2",
    "S1",
    "S2",
    "S3"
  )
  out
}

#' Degroot SFC calculator
#'
#' @param bui
#' @param ffl
#' @param fwfl
#'
#' @returns
#' @export
#'
#' @examples
tool_sfc_degroot <- function(bui = 85, ffl = 3.5, fwfl = 0.3) {
  assert_number(bui, lower = 0, upper = 200)
  assert_number(ffl, lower = 1, upper = 5)
  assert_number(fwfl, lower = 0, upper = 2)
  fffc <- -0.176 + 0.156 * ffl + 0.015 * bui
  sfc  <- fwfl + fffc
  out  <- list("Forest Floor Fuel Consumption" = fffc, "SFC" = sfc)
  out
}

#' Standing dead ladder fuels calculator
#'
#' @param consumption
#' @param cl
#' @param fsg
#'
#' @returns
#' @export
#'
#' @examples
ladder_standing_dead <- function(consumption = 0.2, cl = 4, fsg = 6) {
  assert_number(consumption, lower = 0.1, upper = 10)
  assert_number(cl, lower = 0.5, upper = 15)
  assert_number(fsg, lower = 0.5, upper = 20)
  snag_centroid <- if((cl / 2) >= fsg) fsg - 0.5 else cl / 2
  zl            <- fsg - snag_centroid
  scaled_sfc    <- (fsg / zl)^1.5 * consumption * 3.1
  out           <- list(
    "LFSG [m]" = zl,
    "Scaled SFC contribution, small snags [kg/m²]" = scaled_sfc
  )
  out
}

#' Midstory saplings ladder fuel calculator
#'
#' @param hs
#' @param zs
#' @param zp
#' @param lnfl
#' @param sapling_fmc
#' @param actual_sfc
#'
#' @returns
#' @export
#'
#' @examples
ladder_midstory_saplings <- function(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_fmc = 120,
    actual_sfc  = 2.7
) {
  assert_number(hs)
  assert_number(zs)
  assert_number(zp)
  assert_number(lnfl)
  assert_number(sapling_fmc)
  assert_number(actual_sfc)
  cs           <- zs + (hs - zs) / 2
  fsg          <- if((zp - cs) < 0.5) 0.5 else zp - cs
  deltah       <- (16.52 - 0.057 * sapling_fmc) / 16
  sapling_sfcf <- deltah * lnfl * 1.5 * 3.1
  sfc_cs       <- (fsg / zp)^1.5 * actual_sfc
  total_sfcf   <- sapling_sfcf + sfc_cs
  out          <- list(
    "Sapling crown centroid [m]" = cs,
    "FSG [m]" = fsg,
    "Sapling false-SFC" = sapling_sfcf,
    "SFC scaled to crown centroid" = sfc_cs,
    "Total false-SFC" = total_sfcf
  )
  out
}
