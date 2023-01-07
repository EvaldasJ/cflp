##############################
##############################
##### custom-functions.R #####
##############################
##############################

# Custom functions used for data processing and analysis.

# Functions for converting LKS94 to WGS84 and vice versa are developed by 
# Albertas Agejevas https://github.com/alga and adapted to R by 
# Evaldas Jankauskas, jankauskas.ev@gmail.com

####
#### Power function
#### 

uFuncPower <- function(x, y) {
  
  x <- x^y
  return(x)
  
}

####
#### Rounding function
####

uFuncRoundoff <- function(x, y) {
  
  x <- round(x * uFuncPower(10, y)) / uFuncPower(10, y)
  return(x)

}

####
#### WGS-84 to LKS-94
####

uFuncGeoToGrid <- function(lat, lon) {

  distsize <- 3
  
  j <- 0
  units <- 1
  
  latddd <- lat
  latrad <- REdaS::deg2rad(latddd)
  
  londdd <- lon
  lonrad <- REdaS::deg2rad(londdd)
  
  k <- 0.9998
  a <- 6378137
  f <- 1 / 298.257223563
  b <- a * (1 - f)
  e2 <- (a * a - b * b) / (a * a)
  e <- sqrt(e2)
  ei2 <- (a * a - b * b) / (b * b)
  ei <- sqrt(ei2)
  n <- (a - b) / (a + b)
  G <- a * (1 - n) * (1 - n * n) * (1 + (9 / 4) * n * n + (255 / 64) * uFuncPower(n, 4)) * (pi / 180)
  
  w <- londdd - 24
  w <- REdaS::deg2rad(w)
  t <- tan(latrad)
  rho <- a * (1 - e2) / uFuncPower(1 - (e2 * sin(latrad) * sin(latrad)), (3 / 2))
  nu <- a / sqrt(1 - (e2 * sin(latrad) * sin(latrad)))
  
  psi <- nu / rho
  coslat <- cos(latrad)
  sinlat <- sin(latrad)
  
  A0 <- 1 - (e2 / 4) - (3 * e2 * e2 / 64) - (5 * uFuncPower(e2, 3) / 256)
  A2 <- (3 / 8) * (e2 + (e2 * e2 / 4) + (15 * uFuncPower(e2, 3) / 128))
  A4 <- (15 / 256) * (e2 * e2 + (3 * uFuncPower(e2, 3) / 4))
  A6 <- 35 * uFuncPower(e2, 3) / 3072
  m <- a * ((A0 * latrad) - (A2 * sin(2 * latrad)) + (A4 * sin(4 * latrad)) - (A6 * sin(6 * latrad)))
  
  eterm1 <- (w * w / 6) * coslat * coslat * (psi - t * t)
  eterm2 <- (uFuncPower(w, 4) / 120) * uFuncPower(coslat, 4) * (4 * uFuncPower(psi, 3) * (1 - 6 * t * t) + psi * psi * (1 + 8 * t * t) - psi * 2 * t * t + uFuncPower(t, 4))
  eterm3 <- (uFuncPower(w, 6) / 5040) * uFuncPower(coslat, 6) * (61 - 479 * t * t + 179 * uFuncPower(t, 4) - uFuncPower(t, 6))
  dE <- k * nu * w * coslat * (1 + eterm1 + eterm2 + eterm3)
  east <- uFuncRoundoff(500000 + (dE / units), distsize)
  
  nterm1 <- (w * w / 2) * nu * sinlat * coslat
  nterm2 <- (uFuncPower(w, 4) / 24) * nu * sinlat * uFuncPower(coslat, 3) * (4 * psi * psi + psi - t * t)
  nterm3 <- (uFuncPower(w, 6) / 720) * nu * sinlat * uFuncPower(coslat, 5) * (8 * uFuncPower(psi, 4) * (11 - 24 * t * t) - 28 * uFuncPower(psi, 3) * (1 - 6 * t * t) + psi * psi * (1 - 32 * t * t) - psi * 2 * t * t + uFuncPower(t, 4))
  nterm4 <- (uFuncPower(w, 8) / 40320) * nu * sinlat * uFuncPower(coslat, 7) * (1385 - 3111 * t * t + 543 * uFuncPower(t, 4) - uFuncPower(t, 6))
  dN <- k * (m + nterm1 + nterm2 + nterm3 + nterm4)
  north <- uFuncRoundoff(0 + (dN / units), distsize)
  
  output <- list(east, north)
  return(output)
  
}

####
#### LKS-94 to WGS-84
####

uFuncGridToGeo <- function(x, y) {
  
  distsize <- 3
  
  j <- 0
  units <- 1
  
  k <- 0.9998  		
  a <- 6378137  		
  f <- 1 / 298.257223563  		
  b <- a * (1 - f) 		
  e2 <- (a * a - b * b) / (a * a)  		
  e <- sqrt(e2)  		
  ei2 <- (a * a - b * b) / (b * b) 		
  ei <- sqrt(ei2) 		
  n <- (a - b) / (a + b)
  G <- a * (1 - n) * (1 - n * n) * (1 + (9 / 4) * n * n + (255 / 64) * uFuncPower(n, 4)) * (pi / 180)
  north <- (y - 0) * units  		
  east <- (x - 500000) * units 	
  m <- north / k  		
  sigma <- (m * pi) / (180 * G)
  
  footlat <- sigma + ((3 * n / 2) - (27 * uFuncPower(n, 3) / 32)) * sin(2 * sigma) + ((21 * n * n / 16) - (55 * uFuncPower(n, 4) / 32)) * sin(4 * sigma) + (151 * uFuncPower(n, 3) / 96) * sin(6 * sigma) + (1097 * uFuncPower(n, 4) / 512) * sin(8 * sigma)
  rho <- a * (1 - e2) / uFuncPower(1 - (e2 * sin(footlat) * sin(footlat)), (3 / 2)) 		
  nu <- a / sqrt(1 - (e2 * sin(footlat) * sin(footlat))) 		
  psi <- nu / rho
  t <- tan(footlat)  				
  x <- east / (k * nu)
  laterm1 <- (t / (k * rho )) * ( east * x / 2)
  laterm2 <- (t / (k * rho )) * ( east * uFuncPower(x, 3) / 24) * (-4 * psi * psi + 9 * psi * (1 - t * t) + 12 * t * t )
  laterm3 <- (t / (k * rho )) * ( east * uFuncPower(x, 5) / 720) * (8 * uFuncPower(psi, 4) * (11 - 24 * t * t) - 12 * uFuncPower(psi, 3) * (21 - 71 * t * t) + 15 * psi * psi * (15 - 98 * t * t + 15 * uFuncPower(t, 4)) + 180 * psi * (5 * t * t - 3 * uFuncPower(t, 4)) + 360 * uFuncPower(t, 4))
  laterm4 <- (t / (k * rho )) * ( east * uFuncPower(x, 7) / 40320) * (1385 + 3633 * t * t + 4095 * uFuncPower(t, 4) + 1575 * uFuncPower(t, 6))
  latrad <- footlat - laterm1 + laterm2 - laterm3 + laterm4
  
  lat_deg <- REdaS::rad2deg(latrad)
  
  seclat <- 1 / cos(footlat)
  loterm1 <- x * seclat
  loterm2 <- (uFuncPower(x, 3) / 6) * seclat * (psi + 2 * t * t)
  loterm3 <- (uFuncPower(x, 5) / 120) * seclat * (-4 * uFuncPower(psi, 3) * (1 - 6 * t * t) + psi * psi * (9 - 68 * t * t) + 72 * psi * t * t + 24 * uFuncPower(t, 4))
  loterm4 <- (uFuncPower(x, 7) / 5040) * seclat * (61 + 662 * t * t + 1320 * uFuncPower(t, 4) + 720 * uFuncPower(t, 6))
  w <- loterm1 - loterm2 + loterm3 - loterm4
  longrad <- REdaS::deg2rad(24) + w
  
  lon_deg <- REdaS::rad2deg(longrad)
  
  output <- list(lat_deg, lon_deg)
  return(output)

}

####
#### Convert to numeric
####

uFuncToNumeric <- function(x) {as.numeric(gsub(",",".",noquote(x)))}

####
#### Calculate Huff probability
#### 

uFuncHuff <- function(facility, facility_attractiveness, demand_point, distance, alpha = 1, beta = 2){
  
  huff_numerator <- function(facility_attractiveness, alpha, distance, beta){
    return((facility_attractiveness ^ alpha) / (distance ^ beta))
  }
  
  # Numerator
  huff <- mapply(huff_numerator, facility_attractiveness, alpha, distance, beta) 
  
  # Denominator
  sum_huff_location <- aggregate(huff, by = list(demand_point), sum)
  names(sum_huff_location) <- c("demand_point", "sum_huff")
  
  # Merge
  out <- inner_join(data.frame(demand_point, facility, distance, huff), sum_huff_location, by = 'demand_point') 
  
  # Calculate huff probabilities
  out$huff_probability <- with(out, huff / sum_huff)
  
  return(out)
}
