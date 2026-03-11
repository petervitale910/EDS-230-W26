#' Forest Growth ODE Model
#'
#' @param time time point
#' @param C current forest size (kgC)
#' @param params list with r, g, K, canopy_thresh
#' @return list with dC/dt

forest_growth <- function(time, C, params) {
  with(as.list(c(C, params)), {
    if (C < canopy_thresh) {
      dC <- r * C
    } else {
      dC <- g * (1 - C / K)
    }
    return(list(dC))
  })
}
