
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Analysis code for
# Age differences in the precision of memory at short and long delays
# Rhodes, Abbene, Meierhofer, & Naveh-Benjamin
# 
# Fit the mixture model described in the manuscript
# to the recall data from younger and older adults
# in Experiment 2
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

e2 = read.csv("data/exp2/exp2_cleaned.csv")

e2 = subset(e2, fullObs==1)

mci = function(x) c(m = mean(x), quantile(x, probs = c(.025, .5, .975)))

library(rstan)
library(loo)

rstan_options(auto_write = TRUE)

# used to calculate the area for guessing
rad=(500 - 100*sqrt(2)/2)/100; inner=100*sqrt(2)/100
area = (pi*(rad^2 - inner^2))

# design matrix
ccm = rbind(c(1,0,1), c(1,-1,-1), c(1,1,0)) # r1=short, r2=medium, r3=long

## fit model only accounting for delay ----
if (!file.exists("analysis/stan-model/model-fits/e2-y-fit.rds")){
  for (g in 1:2){
    data_tmp = subset(e2, group==g)
    
    contrasts(data_tmp$delay) = cbind(c(1,-1,0), c(0,-1,1))
    X = model.matrix(~ delay, data_tmp)
    
    data_list = list(
      N = nrow(data_tmp),
      J = length(unique(data_tmp$ID)),
      j = as.numeric(as.factor(data_tmp$ID)),
      p_x = data_tmp$studiedX/100,
      p_y = data_tmp$studiedY/100,
      r_x = data_tmp$recalledX/100,
      r_y = data_tmp$recalledY/100,
      area = area,
      
      P = ncol(X),
      C = nrow(ccm),
      x_m = X,
      x_s = X,
      cont_m = ccm,
      cont_s = ccm
    )
    
    fit_tmp <- stan(
      file = "analysis/stan-model/mixture-2D.stan",
      data = data_list,
      chains = 4,
      warmup = 1000,
      iter = 3000,
      cores = 4
    )
    
    assign(paste0(c("younger", "older")[g], "_fit"), fit_tmp)
    
    rm(list=c("data_tmp", "data_list", "fit_tmp"))
  }
  
  saveRDS(younger_fit, file="analysis/stan-model/model-fits/e2-y-fit.rds")
  saveRDS(older_fit, file="analysis/stan-model/model-fits/e2-o-fit.rds")
} else {
  younger_fit = readRDS("analysis/stan-model/model-fits/e2-y-fit.rds")
  older_fit = readRDS("analysis/stan-model/model-fits/e2-o-fit.rds")
}

summ_older = summary(older_fit)
summ_younger = summary(younger_fit)

all(summ_older$summary[,"Rhat"] < 1.1, na.rm = T)
all(summ_younger$summary[,"Rhat"] < 1.1, na.rm = T)

max(summ_older$summary[,"Rhat"], na.rm = T)
max(summ_younger$summary[,"Rhat"], na.rm = T)

# if we want to eventually compare models (also a good check for outliers...)

log_lik_older_1 = extract_log_lik(older_fit, merge_chains = F)
log_lik_younger_1 = extract_log_lik(younger_fit, merge_chains = F)

r_eff_older_1 = relative_eff(exp(log_lik_older_1))
r_eff_younger_1 = relative_eff(exp(log_lik_younger_1))

loo_older_1 = loo(log_lik_older_1, r_eff = r_eff_older_1)
loo_younger_1 = loo(log_lik_younger_1, r_eff = r_eff_younger_1)
# pareto k's are good

plot(older_fit, pars=c("beta_m", "beta_s"))
plot(younger_fit, pars=c("beta_m", "beta_s"))

traceplot(older_fit, pars=c("beta_m", "beta_s"))
traceplot(younger_fit, pars=c("beta_m", "beta_s"))

traceplot(older_fit, pars=c("tau_m", "tau_s"))
traceplot(younger_fit, pars=c("tau_m", "tau_s"))

## only save the crucial group level parameters...
older_out <- as.matrix(older_fit, pars = c("m_out", "s_out"))
younger_out <- as.matrix(younger_fit, pars = c("m_out", "s_out"))

out = list("older" = older_out, "younger" = younger_out)

saveRDS(out, file="analysis/stan-model/e2-out.rds")

# out = readRDS("analysis/stan-model/e2-out.rds")
# 
# older_out = out$older
# younger_out = out$younger

## manifest scale
older_pmem <- plogis(older_out[,1:3])
older_sigma <- exp(older_out[,4:6])

younger_pmem <- plogis(younger_out[,1:3])
younger_sigma <- exp(younger_out[,4:6])

apply(younger_pmem, 2, FUN = mci)
apply(older_pmem, 2, FUN = mci)

apply(younger_sigma, 2, FUN = mci)
apply(older_sigma, 2, FUN = mci)

## pmem
# short vs med
mci(younger_pmem[,1] - younger_pmem[,2])
mci(older_pmem[,1] - older_pmem[,2])

# short vs long
mci(younger_pmem[,1] - younger_pmem[,3])
mci(older_pmem[,1] - older_pmem[,3])

mci(apply(younger_pmem, 1, mean))
mci(apply(older_pmem, 1, mean))

## sigma
# short vs med
mci(younger_sigma[,1] - younger_sigma[,2])
mci(older_sigma[,1] - older_sigma[,2])

# short vs long
mci(younger_sigma[,1] - younger_sigma[,3])
mci(older_sigma[,1] - older_sigma[,3])

mci(apply(younger_sigma, 1, mean))
mci(apply(older_sigma, 1, mean))

## desity plots
# pmem
plot(density(younger_pmem[,1]), xlim=c(.5,1), main="", xlab="", ylab="")
lines(density(younger_pmem[,2]))
lines(density(younger_pmem[,3]))

lines(density(older_pmem[,1]), col="darkgrey")
lines(density(older_pmem[,2]), col="darkgrey")
lines(density(older_pmem[,3]), col="darkgrey")

# sigma
plot(density(younger_sigma[,1]), xlim=c(.2,.8), main="", xlab="", ylab="")
lines(density(younger_sigma[,2]))
lines(density(younger_sigma[,3]))

lines(density(older_sigma[,1]), col="darkgrey")
lines(density(older_sigma[,2]), col="darkgrey")
lines(density(older_sigma[,3]), col="darkgrey")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## a model with the effect of interval ----

# just models the main effect as interval interactions with
# delay were inconsistent

ccm = rbind(
  c(1,0,1, -1), # short - iei 0
  c(1,-1,-1, -1), # med - iei 0
  c(1,1,0, -1), # long - iei 0
  c(1,0,1, 1), # short - iei 2
  c(1,-1,-1, 1), # med - iei 2
  c(1,1,0, 1) # long - iei 2
)


if (!file.exists("analysis/stan-model/model-fits/e2-y-fit2.rds")){
  for (g in 1:2){
    data_tmp = subset(e2, group==g)
    
    contrasts(data_tmp$delay) = cbind(c(1,-1,0), c(0,-1,1))
    contrasts(data_tmp$interEventInt) = c(-1,1)
    X = model.matrix(~ delay + interEventInt, data_tmp)
    
    data_list = list(
      N = nrow(data_tmp),
      J = length(unique(data_tmp$ID)),
      j = as.numeric(as.factor(data_tmp$ID)),
      p_x = data_tmp$studiedX/100,
      p_y = data_tmp$studiedY/100,
      r_x = data_tmp$recalledX/100,
      r_y = data_tmp$recalledY/100,
      area = area,
      
      P = ncol(X),
      C = nrow(ccm),
      x_m = X,
      x_s = X,
      cont_m = ccm,
      cont_s = ccm
    )
    
    fit_tmp <- stan(
      file = "analysis/stan-model/mixture-2D.stan",
      data = data_list,
      chains = 4,
      warmup = 1000,
      iter = 3000,
      cores = 4
    )
    
    assign(paste0(c("younger", "older")[g], "_fit2"), fit_tmp)
    
    rm(list=c("data_tmp", "data_list", "fit_tmp"))
  }
  
  saveRDS(younger_fit2, file="analysis/stan-model/model-fits/e2-y-fit2.rds")
  saveRDS(older_fit2, file="analysis/stan-model/model-fits/e2-o-fit2.rds")
} else {
  younger_fit2 = readRDS("analysis/stan-model/model-fits/e2-y-fit2.rds")
  older_fit2 = readRDS("analysis/stan-model/model-fits/e2-o-fit2.rds")
}


summ_older2 = summary(older_fit2)
summ_younger2 = summary(younger_fit2)

all(summ_older2$summary[,"Rhat"] < 1.1, na.rm = T)
all(summ_younger2$summary[,"Rhat"] < 1.1, na.rm = T)

max(summ_older2$summary[,"Rhat"], na.rm = T)
max(summ_younger2$summary[,"Rhat"], na.rm = T)

# loo
log_lik_older_2 = extract_log_lik(older_fit2, merge_chains = F)
log_lik_younger_2 = extract_log_lik(younger_fit2, merge_chains = F)

r_eff_older_2 = relative_eff(exp(log_lik_older_2))
r_eff_younger_2 = relative_eff(exp(log_lik_younger_2))

loo_older_2 = loo(log_lik_older_2, r_eff = r_eff_older_2)
loo_younger_2 = loo(log_lik_younger_2, r_eff = r_eff_younger_2)
# pareto k's are good

plot(older_fit2, pars=c("beta_m", "beta_s"))
plot(younger_fit2, pars=c("beta_m", "beta_s"))

traceplot(older_fit2, pars=c("beta_m", "beta_s"))
traceplot(younger_fit2, pars=c("beta_m", "beta_s"))

traceplot(older_fit2, pars=c("tau_m", "tau_s"))
traceplot(younger_fit2, pars=c("tau_m", "tau_s"))

## only save the crucial group level parameters...
older_out2 <- as.matrix(older_fit2, pars = c("m_out", "s_out"))
younger_out2 <- as.matrix(younger_fit2, pars = c("m_out", "s_out"))

out2 = list("older" = older_out2, "younger" = younger_out2)

saveRDS(out2, file="analysis/stan-model/e2-out2.rds")

### compare -----
e2_loowaic = list(
  'y_loo' = compare_ic(loo_younger_1, loo_younger_2),
  'o_loo' = compare_ic(loo_older_1, loo_older_2),
  'y_waic' = compare_ic(waic_younger_1, waic_younger_2),
  'o_waic' = compare_ic(waic_older_1, waic_older_2)
)

saveRDS(e2_loowaic, file="analysis/stan-model/e2-loowaic.rds")


