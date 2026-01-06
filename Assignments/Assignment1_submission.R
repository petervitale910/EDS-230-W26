#' Calculate Solar Panel Energy Output
#'
#' @param panel_area 
#' The solar panel area (m^2)
#' @param solar_rad 
#' The annual average solar radiation (kWh m^{-2})
#' @returns
#' The energy produced (kWh)
#' 
#' @example 
#' solar_calculation(30,90)
#' [1] 506.25

solar_calculation <- function(panel_area, solar_rad){
  energy_prod = (panel_area * .25 * solar_rad * .75)
  return(energy_prod)
}

solar_calculation(500, 2000)
