#' Function for converting p values to '< 0.01' (or desired power of 10)
#'
#' @param x Numeric vector
#' @param digits desired number of digits to round to
#' @return character vector with values below rounding limit coerced to `< x`
#' @description This function is best used for transforming numeric pvalue
#' columns into character representation based on desired number of sig figs.
#' Defaults to replacing numeric vectors below 0.01 with '<0.01'
#' @examples
#' data(mtcars)
#'
#' mod = glm(am ~  hp + wt ,
#'           data = mtcars, family = binomial())
#'
#' tab = glm_ORs(mod)
#'
#' tab$p_1d = to_lessthan(tab$p, digits = 1)
#' tab$p_2d = to_lessthan(tab$p, digits = 2)
#'
#' tab
#'
to_lessthan = function(x, digits = 2){
  ifelse(round(x, digits = digits + 1) < 10 ^ (-1 * digits),
         paste0("<0.",strrep(0,times = digits - 1),"1"),
         print_dec(x,digits = digits))
}
