#' crop_yield
#' Calculate crop yield based on environmental variables
#' @param crop Crop name (Must be wrapped in '')
#' @param temp_1 The first temperature in the crops formula (use crop_yield('list') to see what this variable is)
#' @param temp_2 The second temperature in the crops formula (use crop_yield('list') to see what this variable is). May not be in the function, in which case leave blank. 
#' @param precip_1 The first temperature in the crops formula (use crop_yield('list') to see what this variable is)
#' @param precip_2 The second temperature in the crops formula (use crop_yield('list') to see what this variable is). May not be in the function, in which case leave blank. 
#' @returns
#' @export
#' Outputs a single crop yield, use map parameters to output a list
#' @examples
#' 
crop_yield <- function(crop, 
                       temp_1 = 0,
                       temp_2 = 0, 
                       precip_1 = 0, 
                       precip_2 = 0){
  
  crop_dict <- list(
    
    # -----------------------------
    # Crops
    # -----------------------------
    
    "Almonds" = expression(
      (-.015 * temp_1) - (.0046 * temp_1^2) - (.07 * precip_1) + (.0043 * precip_1 ^2) + .28
    ), 
    
    "Wine Grapes" = expression(
      (2.65 *temp_1) - (0.17 * temp_1 ^2) + (4.78 * precip_1) - (4.93 * precip_1 ^2) - (2.24 * precip_2) + (1.54*precip_2^2) - 10.50),
    
    "Table Grapes" = expression(
      (6.93 *temp_1) - (0.197 * temp_1 ^2) + (2.61 * temp_2) - (.157 * temp_2^2) + (.035 * precip_1) + (.024 * precip_1 ^2) + (1.71 * precip_2) - (.673*precip_2^2) - 73.89),
    
    "Oranges" = expression(
      (1.08 * temp_1) - (.2 * temp_1^2) - (4.99 * precip_1) - (1.97 * precip_1 ^2) - 2.47
    ), 
    
    "Walnuts" = expression(
      (.68 * temp_1) - (.0207 * temp_1^2) + (.038 * precip_1) - (.0051 * precip_1 ^2) - 5.83
    ), 
    
    "Avocados" = expression(
      (17.71 *temp_1) - (.029 * temp_1 ^2) + (3.25 * temp_2) - (.14 * temp_2^2) + (1 * precip_1) - (.31 * precip_1 ^2) - 288.09))
  
  # -----------------------------
  # Crops Parameter list
  # -----------------------------
  crop_var_map <- list(
    
    "Almonds" = c(
      temp_1   = "Tn_2 (Minimum temperature, February)",
      precip_1 = "P_1 (Precipitation, January)"
    ),
    
    "Wine Grapes" = c(
      temp_1   = "Tn_4 (Minimum temperature, April)",
      precip_1 = "P_6 (Precipitation, June)",
      precip_2 = "P_-9 (Precipitation, September previous year)"
    ),
    
    "Table Grapes" = c(
      temp_1   = "Tn_7 (Minimum temperature, July)",
      temp_2   = "Tn_4 (Minimum temperature, April)",
      precip_1 = "P_1 (Precipitation, January)",
      precip_2 = "P_-10 (Precipitation, October previous year)"
    ),
    
    "Oranges" = c(
      temp_1   = "Tn_-12 (Minimum temperature, December previous year)",
      precip_1 = "P_5 (Precipitation, May)"
    ),
    
    "Walnuts" = c(
      temp_1   = "Tx_-11 (Maximum temperature, November previous year)",
      precip_1 = "P_2 (Precipitation, February)"
    ),
    
    "Avocados" = c(
      temp_1   = "Tx_-8 (Maximum temperature, August previous year)",
      temp_2   = "Tn_5 (Minimum temperature, May)",
      precip_1 = "P_-10 (Precipitation, October previous year)"
    )
  )
  
  
  
  if (crop == "list") {
    return(
      lapply(names(crop_dict), function(cr) {
        
        expr <- crop_dict[[cr]]
        used_vars <- all.vars(expr)
        
        var_meaning <- crop_var_map[[cr]][used_vars]
        
        list(
          expression = expr,
          variables  = setNames(var_meaning, used_vars)
        )
        
      }) |> setNames(names(crop_dict))
    )
  }
  
  
  # Extract parameters from the dictionary
  expr <- crop_dict[[crop]]
  
  if (is.null(expr)) stop("Species not found in dictionary. Check spelling.")
  
  output = eval(expr,
                envir = list(
                  t1 = t1,
                  t2 = t2,
                  p1 = p1,
                  p2 = p2))
  
  return( 
    output
  )
}