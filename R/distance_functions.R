#' euclidean_dist
#'
#' @param lab1 Vector of L, a*, and b* values for first colour
#' @param lab2 Vector of L, a*, and b* values for second colour

euclidean_dist <- function(lab1, lab2){
  return(sqrt(sum((lab1[1]-lab2[1])^2, (lab1[2]-lab2[2])^2, (lab1[3]-lab2[3])^2)))
}

# TODO: Distance function for colourblind-friendly palettes
#' cmc
#'
#' param lab1 Vector of L, a*, and b* values for first colour
#' param lab2 Vector of L, a*, and b* values for second colour
#'
#' cmc <- function(lab1, lab2){
#'   C1 <- sqrt(lab1[2]^2 + lab1[3]^2)
#'   C2 <- sqrt(lab2[2]^2 + lab2[3]^2)}
