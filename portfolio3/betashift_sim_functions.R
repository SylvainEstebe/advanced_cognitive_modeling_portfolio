simBetaShift <- function(params,nTrials){
  
  FirstRating = ceiling(runif(nTrials)*8)
  GroupRating = ceiling(runif(nTrials)*8)
  
  mu0 = (FirstRating)/9
  muS = (GroupRating)/9
  
  delta_mu = muS - mu0
  
  mode = mu0 + params$k*delta_mu
  conc = params$c
  
  stopifnot(all(conc > 2))
  
  alpha = mode*(conc-2) + 1
  beta = (1-mode)*(conc-2) + 1
  
  SecondRating = rep(NA,nTrials)
  for (i in 1:nTrials){
    SecondRating[i] = ceiling(rbeta(1,alpha[i],beta[i])*8)
  }
  out = tibble(trial = seq(1:nTrials), 
               FirstRating = FirstRating, 
               GroupRating = GroupRating, 
               SecondRating = SecondRating)
  return(out)
}