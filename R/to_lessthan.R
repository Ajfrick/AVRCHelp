#' Function for converting p values to '< 0.01' (or desired power of 10)
#'
#' @param x Numeric vector
#' @param digits desired number of digits to round to
#' @return character vector with values below rounding limit coerced to `< x`
#' @examples
#' TBD
#'
to_lessthan = function(x, digits = 2){
  ifelse(round(x, digits = digits) == 0,
         paste0("<0.",strrep(0,times = digits - 1),"1"),
         print_dec(x,digits = digits))
}
