#' Fahrenheit to Celsius Conversion
#'
#' Convert temperature from degrees Fahrenheit to degrees Celsius.
#'
#' @param F_temp Numeric vector representing temperature(s) in degrees Fahrenheit.
#' @return Numeric vector representing temperature(s) in degrees Celsius.
#' @examples
#' F_to_C(50)
#' F_to_C(c(32, 68, 104))
#' @export
F_to_C <- function(F_temp){
  C_temp <- (F_temp - 32) * 5/9
  return(C_temp)
}

#' Celsius to Fahrenheit Conversion
#'
#' Convert temperature from degrees Celsius to degrees Fahrenheit.
#'
#' @param C_temp Numeric vector representing temperature(s) in degrees Celsius.
#' @return Numeric vector representing temperature(s) in degrees Fahrenheit.
#' @examples
#' C_to_F(20)
#' C_to_F(c(-10, 0, 25))
#' @export
C_to_F <- function(C_temp){
  F_temp <- (C_temp * 9/5) + 32
  return(F_temp)
}

#' Celsius to Kelvin Conversion
#'
#' Convert temperature from degrees Celsius to Kelvin.
#'
#' @param C_temp Numeric vector representing temperature(s) in degrees Celsius.
#' @return Numeric vector representing temperature(s) in Kelvin.
#' @examples
#' C_to_K(0)
#' C_to_K(c(-10, 25, 100))
#' @export
C_to_K <- function(C_temp) {
  K_temp <- C_temp + 273
  return(K_temp)
}

#' Kelvin to Fahrenheit Conversion
#'
#' Convert temperature from Kelvin to degrees Fahrenheit.
#'
#' @param K_temp Numeric vector representing temperature(s) in Kelvin.
#' @return Numeric vector representing temperature(s) in degrees Fahrenheit.
#' @examples
#' K_to_F(273)
#' K_to_F(c(300, 310))
#' @export
K_to_F <- function(K_temp) {
  F_temp <- (K_temp - 273) * 9/5 + 32
  return(F_temp)
}

#' Kelvin to Celsius Conversion
#'
#' Convert temperature from Kelvin to degrees Celsius.
#'
#' @param K_temp Numeric vector representing temperature(s) in Kelvin.
#' @return Numeric vector representing temperature(s) in degrees Celsius.
#' @examples
#' K_to_C(273)
#' K_to_C(c(300, 250))
#' @export
K_to_C <- function(K_temp) {
  C_temp <- K_temp - 273
  return(C_temp)
}

#' Fahrenheit to Kelvin Conversion
#'
#' Convert temperature from degrees Fahrenheit to Kelvin.
#'
#' @param F_temp Numeric vector representing temperature(s) in degrees Fahrenheit.
#' @return Numeric vector representing temperature(s) in Kelvin.
#' @examples
#' F_to_K(32)
#' F_to_K(c(68, 100))
#' @export
F_to_K <- function(F_temp) {
  K_temp <- (F_temp - 32) * 5/9 + 273
  return(K_temp)
}


