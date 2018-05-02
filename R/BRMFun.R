





BRM.pval = function(brmfit){require(tidyverse)

  npars = length(grep("^b_", names(brmfit$fit@sim$samples[[1]])))-1
  samples =brmfit$fit@sim$samples

  if(npars>1){
    draws = rbind(as.data.frame(samples[[1]])[,2:(1+npars)],
                  as.data.frame(samples[[2]])[,2:(1+npars)],
                  as.data.frame(samples[[3]])[,2:(1+npars)],
                  as.data.frame(samples[[4]])[,2:(1+npars)])

  names = colnames(draws)
  out = tibble(names = names)
  chains = length(brmfit$fit@sim$samples)

  pvals = numeric(npars)

  for(i in 1:(npars)){
    coef = out$names[i]
    vals = draws %>% select(which(colnames(draws)==coef))
    test = sum(vals[,1] < 0)/nrow(draws)
    pvals[i] = ifelse(median(vals[,1])>0,test,1-test)*2
  }
  out = out %>% mutate(pvals = pvals)
  return(out)

  }
  if(npars == 1){

    draws = c(as.data.frame(samples[[1]])[,2:(1+npars)],
              as.data.frame(samples[[2]])[,2:(1+npars)],
              as.data.frame(samples[[3]])[,2:(1+npars)],
              as.data.frame(samples[[4]])[,2:(1+npars)])

    coef = names(samples[[1]][2])
    test = sum(draws < 0)/length(draws)
    pvals = ifelse(mean(draws)>0,test,1-test)*2
    c(coef,as.numeric(pvals))
  }
}

### Output regression coefs directly
BRM.tab = function(fit, digits = 2, predlabs,...){require(brms);require(tidyverse);require(sjmisc)

  # form = fit$formula
  coefs = coef(fit)
  pars = dim(coefs[[1]])[3]
  betacos = t(coefs[[1]][1,c(1,3:4),2:pars])

  if(!missing(predlabs)){
    if(length(predlabs) != (pars-1)){
      warning("Improper Number of Predictor Labels")
    }
  }

  if(missing(predlabs)){
    Tabs = tibble(Predictor = rownames(betacos),
                 RegCoef = round(betacos[,1],digits = digits),
                 Lower95 = round(betacos[,2],digits = digits),
                 Upper95 = round(betacos[,3],digits = digits),
                 pValue  = round(BRM.pval(fit)$pvals,digits = max(3, digits)),
                 #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                 #                 "*","")
                 CI = str_c("(",Lower95,",",Upper95,")"),
                 sig = ifelse(pValue<=0.05,"*",""))%>%
      select(Predictor, RegCoef, CI, pValue, sig)
      return(Tabs)
  }else if(!missing(predlabs)){
    Tabs = tibble(Predictor = predlabs,
                  RegCoef = round(betacos[,1],digits = digits),
                  Lower95 = round(betacos[,2],digits = digits),
                  Upper95 = round(betacos[,3],digits = digits),
                  pValue  = round(BRM.pval(fit)$pvals,digits = max(3, digits)),
                  #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                  #                 "*","")
                  CI = str_c("(",Lower95,",",Upper95,")"),
                  sig = ifelse(pValue<=0.05,"*",""))%>%
      select(Predictor, RegCoef, CI, pValue, sig)
    return(Tabs)
  }

}

### Output ORs from logit linkmodel fit


BRM.ors = function(fit, digits = 2, predlabs,...){require(brms);require(tidyverse);require(sjmisc)

  # form = fit$formula
  coefs = coef(fit)
  pars = dim(coefs[[1]])[3]
  betacos = t(coefs[[1]][1,c(1,3:4),2:pars])

  if(!missing(predlabs)){
    if(length(predlabs) != (pars-1)){
      warning("Improper Number of Predictor Labels")
    }
  }

  if(missing(predlabs)){
    ORs = tibble(Predictor = rownames(betacos),
                 OR = round(exp(betacos[,1]),digits = digits),
                 Lower95 = round(exp(betacos[,2]),digits = digits),
                 Upper95 = round(exp(betacos[,3]),digits = digits),
                 pValue  = round(BRM.pval(fit)$pvals,digits = max(3, digits)),
                 #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                 #                 "*","")
                 CI = str_c("(",Lower95,",",Upper95,")"),
                 sig = ifelse(pValue<=0.05,"*",""))%>%
      select(Predictor, OR, CI, pValue, sig)
    return(ORs)
  }else if(!missing(predlabs)){
    ORs = tibble(Predictor = predlabs,
                 OR = round(exp(betacos[,1]),digits = digits),
                 Lower95 = round(exp(betacos[,2]),digits = digits),
                 Upper95 = round(exp(betacos[,3]),digits = digits),
                 pValue  = round(BRM.pval(fit)$pvals,digits = max(3, digits)),
                 #  'p<0.05'   = ifelse((Lower95>=1 & Upper95>=1) | (Lower95 <= 1 & Upper95 <= 1),
                 #                 "*","")
                 CI = str_c("(",Lower95,",",Upper95,")"),
                 sig = ifelse(pValue<=0.05,"*",""))%>%
      select(Predictor, OR, CI, pValue, sig)
    return(ORs)
  }
}

##### Output probabilities from OR/Logit link Model fit


BRM.probs = function(fit, digits = 2, predlabs,...){require(brms);require(tidyverse);require(sjmisc)

  coefs = coef(fit)
  pars = dim(coefs[[1]])[3]
  betacos = t(coefs[[1]][1,c(1,3:4),2:pars])

  if(missing(predlabs)){predlabs = rownames(betacos)}

  probs = tibble(Predictor = predlabs,
                 Prob = round(inv_logit_scaled(betacos[,1]), digits = 2),
                 Lower = round(inv_logit_scaled(betacos[,2]), digits = 2),
                 Upper = round(inv_logit_scaled(betacos[,3]), digits = 2)) %>%
    mutate(probCI = str_c("(",Lower,", ",Upper,")")) %>%
    select(Predictor, Prob, probCI)
  return(probs)
}

Extract.CMV.Time = function(tabs,name = "", ...){require(brms);require(tidyverse);require(sjmisc)
  bnames = c("Det.CMVCMVP","logart2lab2","Det.CMVCMVP:logart2lab2")
  vals = tabs %>% filter(Predictor %in% bnames) %>%
    mutate(Predictor = c("CMV","Time","CMVxTime"),
           RegCoef = as.character(RegCoef),
           pValue = as.character(pValue)) %>%
    unite(Col, RegCoef:sig, sep = " ") %>%
    spread(Predictor,Col) %>%
    mutate(Subtype = name) %>%
    select(Subtype,CMV,Time,CMVxTime)
  return(vals)
}

Mega.Tab.act = function(funlist){require(brms);require(tidyverse);require(sjmisc)
  mods = length(funlist)
  Tab = tibble(Subtype = character(mods),
               CMV  = character(mods),
               Time  = character(mods),
               CMVxTime = character(mods))
  snames = c("CD4", "CD4-Tcm","CD4-Tem","CD4-Tn","CD4-Ttd","CD4-Tscm",
             "CD8", "CD8-Tcm","CD8-Tem","CD8-Tn","CD8-Ttd")
  Val = list()
  Subtype = character(mods)
  CMV = character(mods)
  Time = character(mods)
  CMVxTime = character(mods)

  minitabs = lapply(funlist, FUN = BRM.tab)
  for(i  in 1:mods){
    Val[[i]] = Extract.CMV.Time(minitabs[[i]],snames[i])
    Subtype[i]=Val[[i]][1]
    CMV[i] = Val[[i]][2]
    Time[i] = Val[[i]][3]
    CMVxTime[i] = Val[[i]][4]
  }
  Subtype = unlist(Subtype)
  CMV = unlist(CMV)
  Time = unlist(Time)
  CMVxTime = unlist(CMVxTime)

  Tab = tibble(Subtype = Subtype,
               CMV = CMV,
               Time = Time,
               CMVxTime = CMVxTime)
  return(Tab)
  }



Mega.Tab.pd1 = function(funlist){require(brms);require(tidyverse);require(sjmisc)
  mods = length(funlist)
  Tab = tibble(Subtype = character(mods),
               CMV  = character(mods),
               Time  = character(mods),
               CMVxTime = character(mods))
  snames = c("CD4-Tn","CD4-Tcm","CD4-Tem","CD4-Ttm","CD4-Ttd")

  Val = list()
  Subtype = character(mods)
  CMV = character(mods)
  Time = character(mods)
  CMVxTime = character(mods)

  minitabs = lapply(funlist, FUN = BRM.tab)
  for(i  in 1:mods){
    Val[[i]] = Extract.CMV.Time(minitabs[[i]],snames[i])
    Subtype[i]=Val[[i]][1]
    CMV[i] = Val[[i]][2]
    Time[i] = Val[[i]][3]
    CMVxTime[i] = Val[[i]][4]
  }
  Subtype = unlist(Subtype)
  CMV = unlist(CMV)
  Time = unlist(Time)
  CMVxTime = unlist(CMVxTime)

  Tab = tibble(Subtype = Subtype,
               CMV = CMV,
               Time = Time,
               CMVxTime = CMVxTime)
  return(Tab)
}

