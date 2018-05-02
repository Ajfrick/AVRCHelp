#' Create center with spread data summary for table
#'
#' @param x Numeric Vector
#' @param fun Function for Central Measure (Defaults to median)
#' @param digits number of digits to round
#' @param delim  delimiter of choice
#' @param lower  Lower Limit (defaults to Q1) for str_comb_iqr only
#' @param upper  Upper Limit (defaults to Q3) for str_comb_iqr only
#' @return Character String of the form \code{fun(x) <delim> (Q1,Q3)}
#' @examples
#' x = seq(1,100)
#' str_comb_IQR(x,digits = 3)
#' str_comb_IQR(x, lower = 0.1, upper = 0.85)
#' str_comb_IQR(x^2,fun = median, delim = " - ")
#' str_comb_IQR(x^2,fun = mean, delim = "_")

str_comb_IQR = function(x, fun = median, digits = 2, delim = ",",
                        lower = 0.25, upper = 0.75){
  # library(tidyverse);library(stringr);
  Cent = round(fun(as.numeric(x), na.rm = T), digits = digits)
  Iqr = round(quantile(as.numeric(x), probs = c(lower,upper), na.rm = T), digits = digits)
  return(stringr::str_c(Cent," (", Iqr[1], delim, Iqr[2], ")"))
}
