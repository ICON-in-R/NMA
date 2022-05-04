
#' Create BUGS code from component parts
#' 
#' @param random Random effect model? Logical
#' @param dat NMA object
#' 
create_bugs_code <- function(random, dat) {
  
  effect <- ifelse(random, "RE", "FE")
  type <- dat$data_type

  # combine all elements into a complete BUGS model
  paste0("model {\n",
    create_prior_code(effect, type),
    create_model_code(effect, type),
    create_generated_quantities_code(effect, type),
    "\n}")
}

#
create_prior_code <- function(effect, type) {
  
    script <- 
      c(
    "
    # Define Prior Distributions
    # On tx effect mean
    
    beta[1] <- 0
    for (tt in 2:nTx) {
      beta[tt] ~ dnorm(mu_beta, prec_beta)
    }
    
    # On individual study baseline effect
    for(ss in 1:nStudies) {
      alpha[ss] ~ dnorm(mu_alpha, prec_alpha)
    }\n\n")
  
    if (effect == "RE") {
      script <-
        paste0(script,
    "
    # on random tx effect variance
    sd ~ dunif(0, 5)
    reTau <- 2/pow(sd, 2)
    
    # Define random effect
    for (ss in 1:nStudies) {
      for(tt in 1:nTx) {
        re[ss, tt] ~ dnorm(0, reTau)
      }
    }\n\n")
  }
  
  script
}

#' @importFrom glue glue
#' 
create_model_code <- function(effect, type) {
  
  script <- ""
  
  # hazard ratio data
  if ("hr_data" %in% type) {
    
    re_term <- 
      if (effect == "RE") {
        "+ re[Lstudy[ii], Ltx[ii]] - re[Lstudy[ii], Lbase[ii]]"
      } else {""}
    
    script <- 
      paste0(script,
             glue("
   # For hazard ratio reporting studies
   for(ii in 1:LnObs) {{
    Lmu[ii] <- alpha[Lstudy[ii]]*multi[ii] {re_term} + beta[Ltx[ii]] - beta[Lbase[ii]]
    Lprec[ii] <- 1/pow(Lse[ii],2)
    Lmean[ii] ~ dnorm(Lmu[ii], Lprec[ii])
    
    # Residual deviance for hazard ratio reporting studies
    Ldev[ii] <- pow((Lmean[ii] - Lmu[ii]), 2)*Lprec[ii]
   }\n\n"))
  }
  
  # median time data
  if ("med_data" %in% type) {
    
    re_term <- 
      if (effect == "RE") {
    "+ re[medianStudy[kk], medianTx[kk]] - re[medianStudy[kk], medianBase[kk]]"
      } else {""}
    
    script <- 
      paste0(script,
      glue("# For data reported as median survival times
   for (kk in 1:medianNObs) {{
     medianMu[kk] <- alpha[medianStudy[kk]] {re_term} + beta[medianTx[kk]] - beta[medianBase[kk]]
     prob[kk] <- exp(-median[kk]*exp(medianMu[kk]))
     medianR[kk] ~ dbin(prob[kk],medianN[kk])

    # Deviance for median data
  	rhat[kk] <- prob[kk] * medianN[kk]
  	mediandev[kk] <- 2 * (medianR[kk] * (log(medianR[kk]) - log(rhat[kk])) +
	                 (medianN[kk] - medianR[kk]) * (log(medianN[kk] - medianR[kk]) -
	                  log(medianN[kk] - rhat[kk])))
   }\n"))
  }
  
  # binary outcome data
  if ("surv_bin_data" %in% type) {
    
    re_term <- 
      if (effect == "RE") {
        "+ re[Bstudy[ss], Btx[ss]] - re[Bstudy[ss], Bbase[ss]]"
      } else {""}
    
    script <- 
      paste0(script,
    glue("# For binary data reporting studies
    for (ss in 1:BnObs) {{
      logCumHaz[ss] <- alpha[Bstudy[ss]] {re_term} + beta[Btx[ss]] - beta[Bbase[ss]]
      cumFail[ss] <- 1 - exp(-1*exp(logCumHaz[ss]))
      Br[ss] ~ dbin(cumFail[ss], Bn[ss])
    }"))
  }
    
  script  
}

#
create_generated_quantities_code <- function(effect, type) {
  
  script <- ""
  
  script <- 
    paste0(script,
  "
  # Calculate HRs
  for (hh in 1:nTx) {
    hr[hh] <- exp(beta[hh])
  }
  
  # Ranking plot
  for (ll in 1:nTx) {
    for (mm in 1:nTx) {
      rk[ll, mm] <- equals(ranked(beta[], mm), beta[ll])
    }
  }\n")
  
  # hazard ratio data
  if ("hr_data" %in% type) {
    script <- 
      paste0(script,
  "
  # Total residual deviances
  totLdev <- sum(Ldev[])\n")
  }
  
  if ("med_data" %in% type) {
    script <- 
      paste0(script,
  "totmediandev <- sum(mediandev[])\n")
  }
  
  if ("med_data" %in% type && "hr_data" %in% type) {
    script <- 
      paste0(script,
  "totresdev <- totmediandev + totLdev")
  }
  
  script
}

