#' xyz_to_rgb
#' xyz_to_rgb corrects for gamma to convert linear RGB to sRGB
#' @param u The intensity of one component (R, G, or B) of a colour defined in RGB

xyz_to_rgb <- function(u){
  if(u <= 0.00304){
    return(round(255 * 12.92 * u))
  } else {
    return(round(255 * (1.055 * u^(1/2.4) - 0.055)))
  }
}

