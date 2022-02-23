xyz_to_rgb <- function(u){
  if(u <= 0.00304){
    return(round(255 * 12.92 * u))
  } else {
    return(round(255 * (1.055 * u^(1/2.4) - 0.055)))
  }
}

xyz_to_lab <- function(t){
  if(t > 0.008856452){
    return(t^(1/3))
  }
  return(t / 0.12841855 + 0.137931034)
}

rgb_to_xyz <- function(rgb){
  rgb <- sapply(rgb, function(x){
    x <- x/255
    if(x <= 0.04045){
      return(x / 12.92)
    } else {
      return(((x+0.055)/1.055)^2.4)
    }
  }, USE.NAMES = FALSE)
  
  r <- rgb[1]
  g <- rgb[2]
  b <- rgb[3]
  
  x <- xyz_to_lab((0.4124564*r + 0.3575761*g + 0.1804375*b) / 0.95047)
  y <- xyz_to_lab((0.2126729*r + 0.7151522*g + 0.0721750*b) / 1)
  z <- xyz_to_lab((0.0193339*r + 0.1191920*g + 0.9503041*b) / 1.08883)
  return(c(x,y,z))
}


lab_to_xyz <- function(t){
  if(t > 0.206896552){
    return(t^3)
  }
  return(0.12841855 * (t - 0.137931034))
}


lab_to_rgb <- function(lab){
  l <- lab[1]
  a <- lab[2]
  b <- lab[3]
  
  y <- (l + 16)/116
  if(is.na(a)){
    x <- y
  } else {
    x <- y + a/500
  }
  if(is.na(b)){
    z <- y
  } else {
    z <- y - b/200
  }
  
  y <- 1 * lab_to_xyz(y)
  x <- 0.95047 * lab_to_xyz(x)
  z <- 1.08883 * lab_to_xyz(z)
  
  r <- xyz_to_rgb(3.2404542*x - 1.5371385*y - 0.4985314*z)
  g <- xyz_to_rgb(-0.969266*x + 1.8760108*y + 0.041556*z)
  b <- xyz_to_rgb(0.0556434*x - 0.2040259*y + 1.0572252*z)
  
  return(c(r,g,b))
}

rgb_to_lab <- function(rgb){
  xyz <- rgb_to_xyz(rgb)
  x <- xyz[1]
  y <- xyz[2]
  z <- xyz[3]
  
  l <- 116 * y - 16
  if(l < 0){
    l <- 0
  }
  return(c(l, 500*(x - y), 200*(y - z)))
}

validate_rgb <- function(rgb){
  return(all(sapply(rgb, function(x){x >= 0 && x <= 255}, USE.NAMES = F)))
}

lab_to_rgb_hex <- function(lab){
  rgb <- lab_to_rgb(lab)
  return(paste0("#", cat(as.character.hexmode(rgb))))
}

  