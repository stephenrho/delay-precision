
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Analysis code for
# Age differences in the precision of memory at short and long delays
# Rhodes, Abbene, Meirehofer, & Naveh-Benjamin
# 
# Fit the mixture model described in the manuscript
# to the recall data from younger and older adults
# for both experiments combined
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

e1 = read.csv("data/exp1/exp1_cleaned.csv")
e1 = subset(e1, fullObs==1)

e2 = read.csv("data/exp2/exp2_cleaned.csv")
e2 = subset(e2, fullObs==1)

e1$exper = 1
e2$exper = 2

both = rbind(e1[c("ID", "lag", "delay", "group", "studiedX", "studiedY", "recalledX", "recalledY", "recallError", "exper", "image")], e2[c("ID", "lag", "delay", "group", "studiedX", "studiedY", "recalledX", "recalledY", "recallError", "exper", "image")])
both$ID = with(both, paste(exper, ID, sep = "-"))


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
if (!file.exists("analysis/stan-model/model-fits/both-y-fit.rds")){
  for (g in 1:2){
    data_tmp = subset(both, group==g)
    
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
  
  saveRDS(younger_fit, file="analysis/stan-model/model-fits/both-y-fit.rds")
  saveRDS(older_fit, file="analysis/stan-model/model-fits/both-o-fit.rds")
} else {
  younger_fit = readRDS("analysis/stan-model/model-fits/both-y-fit.rds")
  older_fit = readRDS("analysis/stan-model/model-fits/both-o-fit.rds")
}

summ_older = summary(older_fit)
summ_younger = summary(younger_fit)

all(summ_older$summary[,"Rhat"] < 1.1, na.rm = T)
all(summ_younger$summary[,"Rhat"] < 1.1, na.rm = T)

max(summ_older$summary[,"Rhat"], na.rm = T)
max(summ_younger$summary[,"Rhat"], na.rm = T)

# loo
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

saveRDS(out, file="analysis/stan-model/both-out.rds")

# out = readRDS("analysis/stan-model/both-out.rds")
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

### 
# pmem
plot(density(younger_pmem[,1]), xlim=c(.5,1), main="", xlab="", ylab="")
lines(density(younger_pmem[,2]))
lines(density(younger_pmem[,3]))

lines(density(older_pmem[,1]), col="darkgrey")
lines(density(older_pmem[,2]), col="darkgrey")
lines(density(older_pmem[,3]), col="darkgrey")

# sigma
plot(density(younger_sigma[,1]), xlim=c(.3,.8), main="", xlab="", ylab="")
lines(density(younger_sigma[,2]))
lines(density(younger_sigma[,3]))

lines(density(older_sigma[,1]), col="darkgrey")
lines(density(older_sigma[,2]), col="darkgrey")
lines(density(older_sigma[,3]), col="darkgrey")


### models by lag -----

## effects code lag (more parameters)

both$lag_fac = as.factor(both$lag)

cmat = cbind(
  c(-1,1,0,0,0,0,0,0,0),
  c(-1,0,1,0,0,0,0,0,0),
  c(-1,0,0,1,0,0,0,0,0),
  c(-1,0,0,0,1,0,0,0,0),
  c(-1,0,0,0,0,1,0,0,0),
  c(-1,0,0,0,0,0,1,0,0),
  c(-1,0,0,0,0,0,0,1,0),
  c(-1,0,0,0,0,0,0,0,1)
)

ccm = cbind(rep(1, nrow(cmat)), cmat)

if (!file.exists("analysis/stan-model/model-fits/both-y-eclag-fit.rds")){
  for (g in 1:2){
    data_tmp = subset(both, group==g)
    
    contrasts(data_tmp$lag_fac) = cmat
    X = model.matrix(~ lag_fac, data_tmp)
    
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
    
    assign(paste0(c("younger", "older")[g], "_eclag_fit"), fit_tmp)
    
    rm(list=c("data_tmp", "data_list", "fit_tmp"))
  }
  
  saveRDS(younger_eclag_fit, file="analysis/stan-model/model-fits/both-y-eclag-fit.rds")
  saveRDS(older_eclag_fit, file="analysis/stan-model/model-fits/both-o-eclag-fit.rds")
} else {
  younger_eclag_fit = readRDS("analysis/stan-model/model-fits/both-y-eclag-fit.rds")
  older_eclag_fit = readRDS("analysis/stan-model/model-fits/both-o-eclag-fit.rds")
}

summ_older_eclag = summary(older_eclag_fit)
summ_younger_eclag = summary(younger_eclag_fit)

all(summ_older_eclag$summary[,"Rhat"] < 1.1, na.rm = T)
all(summ_younger_eclag$summary[,"Rhat"] < 1.1, na.rm = T)

max(summ_older_eclag$summary[,"Rhat"], na.rm = T)
max(summ_younger_eclag$summary[,"Rhat"], na.rm = T)

# loo
log_lik_older_eclag = extract_log_lik(older_eclag_fit, merge_chains = F)
log_lik_younger_eclag = extract_log_lik(younger_eclag_fit, merge_chains = F)

r_eff_older_eclag = relative_eff(exp(log_lik_older_eclag))
r_eff_younger_eclag = relative_eff(exp(log_lik_younger_eclag))

loo_older_eclag = loo(log_lik_older_eclag, r_eff = r_eff_older_eclag)
loo_younger_eclag = loo(log_lik_younger_eclag, r_eff = r_eff_younger_eclag)
# pareto k's are ok

plot(older_eclag_fit, pars=c("beta_m", "beta_s"))
plot(younger_eclag_fit, pars=c("beta_m", "beta_s"))

traceplot(older_eclag_fit, pars=c("beta_m", "beta_s"))
traceplot(younger_eclag_fit, pars=c("beta_m", "beta_s"))

traceplot(older_eclag_fit, pars=c("tau_m", "tau_s"))
traceplot(younger_eclag_fit, pars=c("tau_m", "tau_s"))

## only save the crucial group level parameters...
older_out_eclag <- as.matrix(older_eclag_fit, pars = c("m_out", "s_out"))
younger_out_eclag <- as.matrix(younger_eclag_fit, pars = c("m_out", "s_out"))

out_eclag = list("older" = older_out_eclag, "younger" = younger_out_eclag)

saveRDS(out_eclag, file="analysis/stan-model/both-out-eclag.rds")

## manifest
older_pmem_eclag <- plogis(older_out_eclag[,1:9])
older_sigma_eclag <- exp(older_out_eclag[,10:18])

younger_pmem_eclag <- plogis(younger_out_eclag[,1:9])
younger_sigma_eclag <- exp(younger_out_eclag[,10:18])

## density plots
# pmem
plot(NA, xlim=c(.5,1), ylim=c(0,85), main="", xlab="", ylab="")
for ( i in 1:9){
  lines(density(younger_pmem_eclag[,i]))
  lines(density(older_pmem_eclag[,i]), col="darkgrey")
}

# sigma
plot(NA, xlim=c(.2,.8), ylim=c(0,30), main="", xlab="", ylab="")
for ( i in 1:9){
  lines(density(younger_sigma_eclag[,i]))
  lines(density(older_sigma_eclag[,i]), col="darkgrey")
}


## log(lag + 1) (fewer parameters)

# design matrix for log(lag+1)
ccm = cbind(rep(1, 9), log(c(0,1,2,10,11,12,23,24,25) + 1))

if (!file.exists("analysis/stan-model/model-fits/both-y-loglag-fit.rds")){
  for (g in 1:2){
    data_tmp = subset(both, group==g)
    
    X = model.matrix(~ log(lag + 1), data_tmp)
    
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
    
    assign(paste0(c("younger", "older")[g], "_loglag_fit"), fit_tmp)
    
    rm(list=c("data_tmp", "data_list", "fit_tmp"))
  }
  
  saveRDS(younger_loglag_fit, file="analysis/stan-model/model-fits/both-y-loglag-fit.rds")
  saveRDS(older_loglag_fit, file="analysis/stan-model/model-fits/both-o-loglag-fit.rds")
} else {
  younger_loglag_fit = readRDS("analysis/stan-model/model-fits/both-y-loglag-fit.rds")
  older_loglag_fit = readRDS("analysis/stan-model/model-fits/both-o-loglag-fit.rds")
}

summ_older_loglag = summary(older_loglag_fit)
summ_younger_loglag = summary(younger_loglag_fit)

all(summ_older_loglag$summary[,"Rhat"] < 1.1, na.rm = T)
all(summ_younger_loglag$summary[,"Rhat"] < 1.1, na.rm = T)

max(summ_older_loglag$summary[,"Rhat"], na.rm = T)
max(summ_younger_loglag$summary[,"Rhat"], na.rm = T)

# loo
log_lik_older_loglag = extract_log_lik(older_loglag_fit, merge_chains = F)
log_lik_younger_loglag = extract_log_lik(younger_loglag_fit, merge_chains = F)

r_eff_older_loglag = relative_eff(exp(log_lik_older_loglag))
r_eff_younger_loglag = relative_eff(exp(log_lik_younger_loglag))

loo_older_loglag = loo(log_lik_older_loglag, r_eff = r_eff_older_loglag)
loo_younger_loglag = loo(log_lik_younger_loglag, r_eff = r_eff_younger_loglag)
# pareto k's are good

plot(older_loglag_fit, pars=c("beta_m", "beta_s"))
plot(younger_loglag_fit, pars=c("beta_m", "beta_s"))

traceplot(older_loglag_fit, pars=c("beta_m", "beta_s"))
traceplot(younger_loglag_fit, pars=c("beta_m", "beta_s"))

traceplot(older_loglag_fit, pars=c("tau_m", "tau_s"))
traceplot(younger_loglag_fit, pars=c("tau_m", "tau_s"))

## only save the crucial group level parameters...
older_out_loglag <- as.matrix(older_loglag_fit, pars = c("m_out", "s_out"))
younger_out_loglag <- as.matrix(younger_loglag_fit, pars = c("m_out", "s_out"))

out_loglag = list("older" = older_out_loglag, "younger" = younger_out_loglag)

saveRDS(out_loglag, file="analysis/stan-model/both-out-loglag.rds")

## manifest scale
older_pmem_loglag <- plogis(older_out_loglag[,1:9])
older_sigma_loglag <- exp(older_out_loglag[,10:18])

younger_pmem_loglag <- plogis(younger_out_loglag[,1:9])
younger_sigma_loglag <- exp(younger_out_loglag[,10:18])

## density plots
# pmem
plot(NA, xlim=c(.5,1), ylim=c(0,85), main="", xlab="", ylab="")
for ( i in 1:9){
  lines(density(younger_pmem_loglag[,i]))
  lines(density(older_pmem_loglag[,i]), col="darkgrey")
}

# sigma
plot(NA, xlim=c(.2,.8), ylim=c(0,40), main="", xlab="", ylab="")
for ( i in 1:9){
  lines(density(younger_sigma_loglag[,i]))
  lines(density(older_sigma_loglag[,i]), col="darkgrey")
}
