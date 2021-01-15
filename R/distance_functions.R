#' euclidean_dist
#'
#' @param lab1 Vector of L, a*, and b* values for first colour
#' @param lab2 Vector of L, a*, and b* values for second colour

euclidean_dist <- function(lab1, lab2){
  return(sqrt(sum((lab1[1]-lab2[1])^2, (lab1[2]-lab2[2])^2, (lab1[3]-lab2[3])^2)))
}
