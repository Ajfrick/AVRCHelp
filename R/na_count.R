#' Count Missing and \% of Total
#'
#' @param x Vector
#' @return N (\%) of Number missing
#' @examples
#' x = 1:100
#' ind = sample(1:100, 10, replace = F)
#' x[ind] = NA
#' na_count(x)
#'
#' x = 1:200
#' x[ind] = NA
#' na_count(x)
#'



na_count = function(x){
  return(paste0(sum(is.na(x)), " (",
               round(sum(is.na(x))/length(x)*100, digits = 1),
               ")"))
}
