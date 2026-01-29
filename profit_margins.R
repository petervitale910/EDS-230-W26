#' Profit Margins
#'
#' @param crop 
#' Crop that you wish to analyze
#' @param yield_actual 
#' The yield received from the crop
#' @param yield_anomaly 
#' Anomalies in yield (based on last year) 
#' can be found with the `crop_yield` function 
#' @param min_wage 
#' Minimum wage in your county, currently set to the US minimum wage
#'
#' @returns
#' @export
#'
#' @examples
profit_margins <- function(crop, yield_actual = 1, yield_anomaly = 1, 
                           min_wage = 7.25, hours_adj = 0
                           ){
  # Price dictionary
  crop_price <- list(
    
    "Almonds" = c(
      price   = 16000,
      water_use = 8040,
      hours = 45
    ),
    
    "Wine Grapes" = c(
      price   =  2000,
      water_use = 270,
      hours = 40
    ),
    
    "Table Grapes" = c(
      price   = 150,
      water_use = 432,
      hours = 55
    ),
    
    "Oranges" = c(
      price   = 1750,
      water_use = 5808,  # ~22 acre-feet at $264/acre-foot
      hours = 60
    ),
    
    "Walnuts" = c(
      price   = 1720,
      water_use = 5400,  # ~36 acre-feet at $150/acre-foot
      hours = 28
    ),
    
    "Avocados" = c(
      price   = 2500,
      water_use = 8000,  # ~4 acre-feet at $2000/acre-foot
      hours = 45
    )
  )
yield <- yield_actual + yield_anomaly

cost <- crop_price[[crop]][2] 
  
price <- crop_price[[crop]][1]

wage <- min_wage * (crop_price[[crop]][3]+hours_adj)

if (is.null(price)) stop("Species not found in dictionary. Check spelling.")

  # Cost Dictionary (function inside)
  profit <- (yield*price) - ((cost + wage))
}