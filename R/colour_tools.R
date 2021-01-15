#' create_lab
#'
#' create_lab takes a subset RGB colours and converts it to CIE L*a*b* format
#'
#' @param rgb A matrix of colours in RGB format.
#' @export

create_lab <- function(rgb = "full"){
  if(rgb == "full"){
    colours = IWantHueR:::full_rgb
  }
  convertColor(colours, "sRGB", "Lab")
}
