
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Analysis code for
# Age differences in the precision of memory at short and long delays
# Rhodes, Abbene, Meirehofer, & Naveh-Benjamin
# 
# Log normal linear mixed effects analyses using brms &
# analysis of confidence ratings with cumulative model
# for both Experiments 1 and 2
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

library(lme4)
library(brms)

### Analysis of recall error ----

e1_full = read.csv("data/exp1/exp1_cleaned.csv")
e2_full = read.csv("data/exp2/exp2_cleaned.csv")

e1_full$exper = 1
e2_full$exper = 2

e1_full = within(e1_full, {
  group = as.factor(group)
  ID = as.factor(ID)
})

e2_full = within(e2_full, {
  group = as.factor(group)
  ID = as.factor(ID)
  interEventInt = as.factor(interEventInt)
})

# Keep full observations
e1 = subset(e1_full, fullObs==1)
e2 = subset(e2_full, fullObs==1)

# set up contrasts, priors, etc
options(contrasts=c("contr.sum", "contr.sum"))
contrasts(e1$delay) = cbind(c(1,-1,0), c(0,-1,1))
contrasts(e1$group)

contrasts(e2$delay) = cbind(c(1,-1,0), c(0,-1,1))
contrasts(e2$group)

options(mc.cores = parallel::detectCores())

priors = c(set_prior("cauchy(0, 2.5)", class = "Intercept"),
           set_prior("cauchy(0, 2.5)", class = "b"),
           set_prior("cauchy(0, 2.5)", class = "sd"))

# rescale error
e1$recallError9 = e1$recallError/100
e2$recallError9 = e2$recallError/100

# combined
both = rbind(e1[c("ID", "lag", "delay", "group", "recallError", "exper", "image")], e2[c("ID", "lag", "delay", "group", "recallError", "exper", "image")])

both$ID = as.factor(with(both, paste(exper, ID, sep = "-")))

both$exper  = as.factor(both$exper)
contrasts(both$exper)
contrasts(both$delay) = cbind(c(1,-1,0), c(0,-1,1))
contrasts(both$group)
both$recallError9 = both$recallError/100

# uncomment the line below if you want to create a directory for rdata files
#dir.create("analysis/rdata-files/")

### run analyses -----
if (!file.exists("analysis/rdata-files/brm-error.RData")){
  
  # experiment 1
  e1_brm = brm(recallError9 | trunc(lb = 0, ub = 9) ~ delay*group + (1 | ID) + (1 | image),
               family =  lognormal(link = "identity"),
               data = e1,
               prior = priors,
               iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  e1_brm2 = brm(log(recallError) ~ delay*group + (1 | ID) + (1 | image),
                family = gaussian(link = "identity"),
                data = e1,
                prior = priors,
                iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  pp_check(e1_brm, nsamples = 100)
  pp_check(e1_brm2, nsamples = 100)
  
  e1_brm_loo = loo(e1_brm)
  e1_brm2_loo = loo(e1_brm2)
  loo_compare(e1_brm_loo, e1_brm2_loo)
  # modeling the log transformation of recall error with a gaussian model
  # is not as good as the log-normal model
  
  marginal_effects(e1_brm)
  
  # experiment 2
  e2_brm = brm(recallError9 | trunc(lb = 0, ub = 9) ~ delay*group*interEventInt + (1 | ID) + (1 | image),
               family =  lognormal(link = "identity"),
               data = e2,
               prior = priors,
               iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  e2_brm2 = brm(log(recallError) ~ delay*group*interEventInt + (1 | ID) + (1 | image),
                family = gaussian(link = "identity"),
                data = e2,
                prior = priors,
                iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  pp_check(e2_brm, nsamples = 100)
  pp_check(e2_brm2, nsamples = 100)
  
  e2_brm_loo = loo(e2_brm)
  e2_brm2_loo = loo(e2_brm2)
  loo_compare(e2_brm_loo, e2_brm2_loo)
  
  marginal_effects(e2_brm)
  
  # test removing the three-way interaction
  e2_brm_noiei = update(e2_brm, .~. -delay:group:interEventInt)
  
  pp_check(e2_brm_noiei, nsamples = 100)
  
  e2_brm_noiei_loo = loo(e2_brm_noiei)
  loo_compare(e2_brm_loo, e2_brm_noiei_loo)
  
  marginal_effects(e2_brm_noiei)
  
  # both experiments together
  both_brm = brm(recallError9 | trunc(lb = 0, ub = 9) ~ delay*group*exper + (1 | ID) + (1 | image),
                 family =  lognormal(link = "identity"),
                 data = both,
                 prior = priors,
                 iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  both_brm2 = brm(log(recallError) ~ delay*group*exper + (1 | ID) + (1 | image),
                  family = gaussian(link = "identity"),
                  data = both,
                  prior = priors,
                  iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  pp_check(both_brm, nsamples = 100)
  pp_check(both_brm2, nsamples = 100)
  
  both_brm_loo = loo(both_brm)
  both_brm2_loo = loo(both_brm2)
  loo_compare(both_brm_loo, both_brm2_loo)
  
  marginal_effects(both_brm)
  
  ## more fine-grained lag analyses
  # effects coded (more parameters)
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
  both$lag_fac = as.factor(both$lag)
  contrasts(both$lag_fac) = cmat
  
  both_brm_eclag = brm(recallError9 | trunc(lb = 0, ub = 9) ~ lag_fac*group + (1 | ID) + (1 | image),
                 family =  lognormal(link = "identity"),
                 data = both,
                 prior = priors,
                 iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  pp_check(both_brm_eclag, nsamples = 100)
  
  both_brm_eclag_loo = loo(both_brm_eclag)
  loo_compare(both_brm_loo, both_brm_eclag_loo)
  
  marginal_effects(both_brm_eclag)
  
  # log lag + 1 (fewer parameters)
  both$loglag1 = log(both$lag + 1)
  
  both_brm_loglag = brm(recallError9 | trunc(lb = 0, ub = 9) ~ loglag1*group + (1 | ID) + (1 | image),
                       family =  lognormal(link = "identity"),
                       data = both,
                       prior = priors,
                       iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  pp_check(both_brm_loglag, nsamples = 100)
  
  marginal_effects(both_brm_loglag)
  
  both_brm_loglag_loo = loo(both_brm_loglag)
  loo_compare(both_brm_loo, both_brm_loglag_loo)
  loo_compare(both_brm_eclag_loo, both_brm_loglag_loo)
  # effects coded lag is better (despite more parameters)
  # than log(lag+1)
  
  # save analyses
  sav_list = ls(pattern="brm")
  save(list=sav_list, file = "analysis/rdata-files/brm-error.RData")
  
} else {
  load("analysis/rdata-files/brm-error.RData")
}


### confidence ratings (experiment 1)

mean(is.na(e1$conf))

e1$conf = 1 + e1$conf

priors_conf = c(set_prior("cauchy(0, 5)", class = "Intercept"),
                set_prior("cauchy(0, 2.5)", class = "b"),
                set_prior("cauchy(0, 2.5)", class = "sd"))

if (!file.exists("analysis/rdata-files/brm-conf.RData")){
  
  e1_conf_brm = brm(conf ~ delay*group + (1 | ID) + (1 | image), 
                    family = cumulative(link = "logit"),
                    data = e1, 
                    prior = priors,
                    iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  pp_check(e1_conf_brm, nsamples = 100)
  
  marginal_effects(e1_conf_brm, categorical = T)
  marginal_effects(e1_conf_brm)
  
  # with(e1, table(group, conf))
  
  e1_conf = subset(e1, !is.na(conf)) # remove obs with missing confidence
  
  e1_conf$conf = as.factor(e1_conf$conf)
  
  # include confidence as a monotonic predictor
  # of recall error
  e1_conf$conf = factor(e1_conf$conf, levels = 6:1, ordered = T)
  
  e1_conf_brm2 = brm(recallError9 | trunc(lb = 0, ub = 9) ~ mo(conf)*delay*group + (1 | ID) + (1 | image),
                     family =  lognormal(link = "identity"),
                     data = e1_conf,
                     prior = priors,
                     iter = 3000, chains = 4, warmup = 1000, cores=options()$mc.cores
  )
  
  marginal_effects(e1_conf_brm2, effects = "group:delay")
  marginal_effects(e1_conf_brm2, effects = "conf:delay", conditions=data.frame(group=c(1,2)))
  marginal_effects(e1_conf_brm2, effects = "delay:conf", conditions=data.frame(group=c(1,2)))
  
  marginal_effects(e1_conf_brm2, effects = "conf", conditions=data.frame(group=c(1,2)))
  
  pp_check(e1_conf_brm2, nsamples = 100)
  
  # save analyses
  sav_list = ls(pattern="conf_brm")
  save(list=sav_list, file = "analysis/rdata-files/brm-conf.RData")
  
} else {
  load("analysis/rdata-files/brm-conf.RData")
}


