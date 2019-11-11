
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Analysis code for
# Age differences in the precision of memory at short and long delays
# Rhodes, Abbene, Meierhofer, & Naveh-Benjamin
# 
# Fit the mixture model described in the manuscript
# to the recall data from younger and older adults
# in Experiment 1
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

e1 = read.csv("data/exp1/exp1_cleaned.csv")

e1 = subset(e1, fullObs==1)

mci = function(x) c(m = mean(x), quantile(x, probs = c(.025, .5, .975)))

library(rstan)
library(loo)

rstan_options(auto_write = TRUE)

# used to calculate the area for guessing
rad=(500 - 100*sqrt(2)/2)/100; inner=100*sqrt(2)/100
area = (pi*(rad^2 - inner^2))

# design matrix
ccm = rbind(c(1,0,1), c(1,-1,-1), c(1,1,0)) # r1=short, r2=medium, r3=long


## loop to fit the model separately to young and older groups

# dir.create("analysis/stan-model/model-fits")
if (!file.exists("analysis/stan-model/model-fits/e1-y-fit.rds")){
  for (g in 1:2){
    data_tmp = subset(e1, group==g)
    
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
  
  saveRDS(younger_fit, file="analysis/stan-model/model-fits/e1-y-fit.rds")
  saveRDS(older_fit, file="analysis/stan-model/model-fits/e1-o-fit.rds")
} else {
  younger_fit = readRDS("analysis/stan-model/model-fits/e1-y-fit.rds")
  older_fit = readRDS("analysis/stan-model/model-fits/e1-o-fit.rds")
}

summ_older = summary(older_fit)
summ_younger = summary(younger_fit)

all(summ_older$summary[,"Rhat"] < 1.1, na.rm = T)
all(summ_younger$summary[,"Rhat"] < 1.1, na.rm = T)

max(summ_older$summary[,"Rhat"], na.rm = T)
max(summ_younger$summary[,"Rhat"], na.rm = T)

# calculate loo (a way of checking for outliers, see Vehtari et al 2017)
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

# traceplot(fit1, pars=c("beta_m", "beta_s"))
# traceplot(fit1, pars=c("m_out", "s_out"))
# traceplot(fit1, pars=c("b_m"))
# traceplot(fit1, pars=c("b_s"))

## only save the crucial group level parameters...
older_out <- as.matrix(older_fit, pars = c("m_out", "s_out"))
younger_out <- as.matrix(younger_fit, pars = c("m_out", "s_out"))

out = list("older" = older_out, "younger" = younger_out)

saveRDS(out, file="analysis/stan-model/e1-out.rds")

# out = readRDS("analysis/stan-model/e1-out.rds")
# older_out = out$older
# younger_out = out$younger


## look at differences between/within age groups
# extract parameters on manifest scale
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
quantile(younger_pmem[,1] - younger_pmem[,2], probs = c(.025, .5, .975))
quantile(older_pmem[,1] - older_pmem[,2], probs = c(.025, .5, .975))

# short vs long
quantile(younger_pmem[,1] - younger_pmem[,3], probs = c(.025, .5, .975))
quantile(older_pmem[,1] - older_pmem[,3], probs = c(.025, .5, .975))

quantile(apply(younger_pmem, 1, mean), probs = c(.025, .5, .975))
quantile(apply(older_pmem, 1, mean), probs = c(.025, .5, .975))

## sigma
# short vs med
quantile(younger_sigma[,1] - younger_sigma[,2], probs = c(.025, .5, .975))
quantile(older_sigma[,1] - older_sigma[,2], probs = c(.025, .5, .975))

# short vs long
quantile(younger_sigma[,1] - younger_sigma[,3], probs = c(.025, .5, .975))
quantile(older_sigma[,1] - older_sigma[,3], probs = c(.025, .5, .975))

quantile(apply(younger_sigma, 1, mean), probs = c(.025, .5, .975))
quantile(apply(older_sigma, 1, mean), probs = c(.025, .5, .975))

### 
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


## high vs low confidence ----
# compare high confidence 3-5 to low confidence 0-2 (see manuscript for details)

e1_conf = subset(e1, !is.na(conf))

e1_conf$confcat = as.factor(ifelse(e1_conf$conf >= 3, "high", "low"))

ccm = rbind(
  c(1,0,1, -1, c(0,1)*-1), # short - low
  c(1,-1,-1, -1, c(-1,-1)*-1), # med - low
  c(1,1,0, -1, c(1,0)*-1), # long - low
  c(1,0,1, 1, c(0,1)*1), # short - high
  c(1,-1,-1, 1, c(-1,-1)*1), # med - high
  c(1,1,0, 1, c(1,0)*1) # long - high
)

if (!file.exists("analysis/stan-model/model-fits/e1-y-conf-fit.rds")){
  for (g in 1:2){
    data_tmp = subset(e1_conf, group==g)
    
    contrasts(data_tmp$delay) = cbind(c(1,-1,0), c(0,-1,1))
    contrasts(data_tmp$confcat) = c(1,-1)
    X = model.matrix(~ delay*confcat, data_tmp)
    
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
    
    assign(paste0(c("younger", "older")[g], "_conf_fit"), fit_tmp)
    
    rm(list=c("data_tmp", "data_list", "fit_tmp"))
  }
  
  saveRDS(younger_conf_fit, file="analysis/stan-model/model-fits/e1-y-conf-fit.rds")
  saveRDS(older_conf_fit, file="analysis/stan-model/model-fits/e1-o-conf-fit.rds")
} else {
  younger_conf_fit = readRDS("analysis/stan-model/model-fits/e1-y-conf-fit.rds")
  older_conf_fit = readRDS("analysis/stan-model/model-fits/e1-o-conf-fit.rds")
}

summ_older_conf = summary(older_conf_fit)
summ_younger_conf = summary(younger_conf_fit)

all(summ_older_conf$summary[,"Rhat"] < 1.1, na.rm = T)
all(summ_younger_conf$summary[,"Rhat"] < 1.1, na.rm = T)

max(summ_older_conf$summary[,"Rhat"], na.rm = T)
max(summ_younger_conf$summary[,"Rhat"], na.rm = T)

# loo
log_lik_older_conf = extract_log_lik(older_conf_fit, merge_chains = F)
log_lik_younger_conf = extract_log_lik(younger_conf_fit, merge_chains = F)

r_eff_older_conf = relative_eff(exp(log_lik_older_conf))
r_eff_younger_conf = relative_eff(exp(log_lik_younger_conf))

loo_older_conf = loo(log_lik_older_conf, r_eff = r_eff_older_conf)
loo_younger_conf = loo(log_lik_younger_conf, r_eff = r_eff_younger_conf)
# pareto k's are ok

plot(older_conf_fit, pars=c("beta_m", "beta_s"))
plot(younger_conf_fit, pars=c("beta_m", "beta_s"))

traceplot(older_conf_fit, pars=c("beta_m", "beta_s"))
traceplot(younger_conf_fit, pars=c("beta_m", "beta_s"))

traceplot(older_conf_fit, pars=c("tau_m", "tau_s"))
traceplot(younger_conf_fit, pars=c("tau_m", "tau_s"))

## only save the crucial group level parameters...
older_out_conf <- as.matrix(older_conf_fit, pars = c("m_out", "s_out"))
younger_out_conf <- as.matrix(younger_conf_fit, pars = c("m_out", "s_out"))

out_conf = list("older" = older_out_conf, "younger" = younger_out_conf)

saveRDS(out_conf, file="analysis/stan-model/e1-out-conf.rds")

## extract on manifest scale
older_pmem_conf <- plogis(older_out_conf[,1:6])
older_sigma_conf <- exp(older_out_conf[,7:12])

younger_pmem_conf <- plogis(younger_out_conf[,1:6])
younger_sigma_conf <- exp(younger_out_conf[,7:12])


apply(younger_pmem_conf, 2, FUN = mci)
apply(older_pmem_conf, 2, FUN = mci)

apply(younger_sigma_conf, 2, FUN = mci)
apply(older_sigma_conf, 2, FUN = mci)

# compare conf
apply(younger_pmem_conf[,1:3]-younger_pmem_conf[,4:6], 2, FUN = mci)
apply(older_pmem_conf[,1:3]-older_pmem_conf[,4:6], 2, FUN = mci)

apply(younger_sigma_conf[,1:3]-younger_sigma_conf[,4:6], 2, FUN = mci)
apply(older_sigma_conf[,1:3]-older_sigma_conf[,4:6], 2, FUN = mci)


## pmem
# short vs med
mci(younger_pmem_conf[,1] - younger_pmem_conf[,2])
mci(older_pmem_conf[,1] - older_pmem_conf[,2])

# short vs long
mci(younger_pmem_conf[,1] - younger_pmem_conf[,3])
quantile(older_pmem_conf[,1] - older_pmem_conf[,3])

mci(apply(younger_pmem_conf, 1, mean))
mci(apply(older_pmem_conf, 1, mean))

## sigma
# short vs med
mci(younger_sigma_conf[,1] - younger_sigma_conf[,2])
mci(older_sigma_conf[,1] - older_sigma_conf[,2])

# short vs long
mci(younger_sigma_conf[,1] - younger_sigma_conf[,3])
mci(older_sigma_conf[,1] - older_sigma_conf[,3])

mci(apply(younger_sigma_conf, 1, mean))
mci(apply(older_sigma_conf, 1, mean))


### 
# pmem
par(mfrow=c(1,2))
plot(density(younger_pmem_conf[,1]), xlim=c(0,1), ylim=c(0,10), main="Low conf", xlab="", ylab="")
lines(density(younger_pmem_conf[,2]))
lines(density(younger_pmem_conf[,3]))

lines(density(older_pmem_conf[,1]), col="darkgrey")
lines(density(older_pmem_conf[,2]), col="darkgrey")
lines(density(older_pmem_conf[,3]), col="darkgrey")

plot(density(younger_pmem_conf[,4]), xlim=c(0.6,1), ylim=c(0,50), main="High conf", xlab="", ylab="")
lines(density(younger_pmem_conf[,5]))
lines(density(younger_pmem_conf[,6]))

lines(density(older_pmem_conf[,4]), col="darkgrey")
lines(density(older_pmem_conf[,5]), col="darkgrey")
lines(density(older_pmem_conf[,6]), col="darkgrey")

# sigma
plot(density(younger_sigma_conf[,1]), xlim=c(.1,1.2), ylim=c(0,8), main="Low conf", xlab="", ylab="")
lines(density(younger_sigma_conf[,2]))
lines(density(younger_sigma_conf[,3]))

lines(density(older_sigma_conf[,1]), col="darkgrey")
lines(density(older_sigma_conf[,2]), col="darkgrey")
lines(density(older_sigma_conf[,3]), col="darkgrey")

plot(density(younger_sigma_conf[,4]), xlim=c(.3,.8), ylim=c(0,30), main="High conf", xlab="", ylab="")
lines(density(younger_sigma_conf[,5]))
lines(density(younger_sigma_conf[,6]))

lines(density(older_sigma_conf[,4]), col="darkgrey")
lines(density(older_sigma_conf[,5]), col="darkgrey")
lines(density(older_sigma_conf[,6]), col="darkgrey")

par(mfrow=c(1,1))

