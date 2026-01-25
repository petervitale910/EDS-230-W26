profit_margins <- function(crop, yield, min_wage = 7.25){
  # Price dictionary
  crop_price <- list(
    
    "Almonds" = c(
      price   = 16000,
      water_use = 1300000,
      hours = 45
    ),
    
    "Wine Grapes" = c(
      price   =  2000,
      water_use = 975000,
      hours = 40
    ),
    
    "Table Grapes" = c(
      price   = 150,
      water_use = 696000,
      hours = 55
    ),
    
    "Oranges" = c(
      price   = 1750,
      water_use = 1828000,
      hours = 60
    ),
    
    "Walnuts" = c(
      price   = 1720,
      water_use = 1610000,
      hours = 28
    ),
    
    "Avocados" = c(
      price   = 2500,
      water_use = 1500000,
      hours = 45
    )
  )
cost <- crop_price[[crop]][2] * 2500 
  
price <- crop_price[[crop]][1]

wage <- min_wage * crop_price[[crop]][3]

if (is.null(price)) stop("Species not found in dictionary. Check spelling.")

  # Cost Dictionary (function inside)
  profit <- (yield*price) - (yield*(cost + wage))
}