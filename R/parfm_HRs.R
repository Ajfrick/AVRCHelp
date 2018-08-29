#' Generate Hazard ratio, CI,  Pval table from parfm
#'
#' @param fit model fit by parfm function
#' @param pred Optional Argument for models with single predictor
#' @description Generates table with Hazard rate, 95 CI and pval for parfm models fixed effects
#' WIP name extraction via other methods, until then names for models with single predictor
#' will have blank in Name column
#' @return Tibble with 5 columns: Name (char), HR, lower CI, Upper CI, p-val (doubles)
#' @examples
#' data(kidney)
#'
#' fit = parfm(Surv(time,status) ~ age + sex + disease,
#'              cluster = "id", data = kidney,
#'              dist = "weibull", frailty = "gamma")
#'
#' parfm_HRs(fit)

parfm_HRs = function(fit, pred = NA){
  CI  = parfm::ci.parfm(fit)
  tab = dplyr::tibble(
    Name= ifelse(is.null(names(coef(fit))),pred,names(coef(fit))),
    HR = exp(coef(fit)),
    low = CI[,1],
    upp = CI[,2],
    p = na.omit(fit[,3])
  )
  tab
}

