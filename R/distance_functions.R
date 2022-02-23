euclidean_dist <- function(lab1, lab2){
  return(sqrt(sum((lab1[1]-lab2[1])^2, (lab1[2]-lab2[2])^2, (lab1[3]-lab2[3])^2)))
}


# lab1 Vector of L, a*, and b* values for first colour
# lab2 Vector of L, a*, and b* values for second colour
# l Weight for lightness. Default = 2
# c Weight for chroma. Default = 1

cmc <- function(lab1, lab2, l=2, c=1){
  L1 <- lab1[1]
  L2 <- lab2[1]
  a1 <- lab1[2]
  a2 <- lab2[2]
  b1 <- lab1[3]
  b2 <- lab2[3]
  C1 <- sqrt(a1^2 + b1^2)
  C2 <- sqrt(a2^2 + b2^2)
  deltaC <- C1 - C2
  deltaL <- L1 - L2
  deltaa <- a1 - a2
  deltab <- b1 - b2
  deltaH <- deltaa^2 + deltab^2 + deltaC^2
  H1 <- atan2(b1, a1) * 180/pi
  while(H1 < 0){
    H1 <- H1 + 360
  }
  f <- sqrt(C1^4/(C1^4 + 1900)) 
  if(H1 >= 164 && H1 <= 345){
    t <- 0.56 + abs(0.2 * cos(H1 + 168))
  } else {
    t <- 0.36 + abs(0.4 * cos(H1 + 35))
  }
  if(L1 < 16){
    S_L <- 0.511
  } else {
    S_L <- (0.040975 * L1) / (1 + 0.01765 * L1)
  }
  S_C <- (0.0638 * C1) / (1 + 0.0131 * C1) + 0.638
  S_H <- S_C * (f * t + 1 - f) 
  return(sqrt((deltaL/l*S_L)^2 + (deltaC/c*S_C)^2 + (deltaH/S_H)^2))
}

