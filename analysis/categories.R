
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Analysis code for
# Age differences in the precision of memory at short and long delays
# Rhodes, Abbene, Meierhofer, & Naveh-Benjamin
# 
# Exploratory analysis using the CatContModel package of
# Hardman 2017 to assess evidence of categorical responding
# in our task (see manuscript and supplement for details).
# Models are fit to the *angle* of the presented and recalled location
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

library(CatContModel)

e1 = read.csv("data/exp1/exp1_cleaned.csv")
e1 = subset(e1, fullObs==1)

e2 = read.csv("data/exp2/exp2_cleaned.csv")
e2 = subset(e2, fullObs==1)

e1$exper = 1
e2$exper = 2

both = rbind(e1[c("ID", "lag", "delay", "group", "studiedX", "studiedY", "recalledX", "recalledY", "recallError", "exper", "image")], e2[c("ID", "lag", "delay", "group", "studiedX", "studiedY", "recalledX", "recalledY", "recallError", "exper", "image")])
both$ID = with(both, paste(exper, ID, sep = "-"))

both$study = with(both, atan2(y = studiedY, x = studiedX))*(180/pi) + 180
both$response = with(both, atan2(y = recalledY, x = recalledX))*(180/pi) + 180

wrap <- function(Y, bound = pi) {
  X <- ((Y + bound) %% (bound*2)) - bound
  return(X)
}

with(both, hist(study))
with(both, hist(response))

with(both, hist(wrap(study - response, 180), breaks=30))

with(both, plot(study, response, pch=16, col=rgb(.2,.2,.2,.1)))

### fit model
cc_cols <- c("pnum", "cond", "study", "response")
cc_both = both[,c("ID", "delay", "study", "response", "group")]
colnames(cc_both)[1:2] <- cc_cols[1:2]

cc_both$group <- ifelse(cc_both$group==1, "Younger", "Older")
  
# put data in a named list
group_data = list()
for (g in unique(cc_both$group)){
  group_data[[g]] <- cc_both[cc_both$group==g, cc_cols]
}

# configs
configs = list()
for (g in names(group_data)){
  configs[[g]] = list(
    modelVariant = "betweenItem",
    iterations = 500
  )
}

configs$Older$factors = data.frame(
  cond = c("short","medium", "long"),
  delay = c("short","medium", "long"),
  agegroup = "Older",
  maxCategories = 12,
  stringsAsFactors = FALSE
)
configs$Younger$factors = data.frame(
  cond = c("short","medium", "long"),
  delay = c("short","medium", "long"),
  agegroup = "Younger",
  maxCategories = 12,
  stringsAsFactors = FALSE
)


# set MH sampling params
mh_settings = list(
  # each of these has been increased slightly to reduce acceptance
  # see the CatContModel documentation for details
  catMu = 6,
  catSD = 2,
  catSD_cond = .3,
  catSelectivity = 9,
  catSelectivity_cond = .8,
  contSD = 2,
  contSD_cond = .7,
  pBetween = .9,
  pBetween_cond = .4,
  pCatGuess = 1.5,
  pCatGuess_cond = .3,
  pContBetween = .7,
  pContBetween_cond = .2,
  pMem = .7,
  pMem_cond = .2
)

# assumes you have created the rdata-files folder
#dir.create("analysis/rdata-files/")

if (!file.exists("analysis/rdata-files/CatCont_results.RDS")){
  # run initial samples
  group_res = list()
  for (g in names(group_data)){
    group_res[[g]] <- runParameterEstimation(
      config = configs[[g]],
      data = group_data[[g]],
      mhTuningOverrides = mh_settings
    )
  }
  
  # look at MH acceptance (if these need improving change MH params above)
  examineMHAcceptance(group_res$Older)
  examineMHAcceptance(group_res$Younger)
  
  saveRDS(group_res, file="analysis/rdata-files/CatCont_results.RDS")
} else {
  group_res = readRDS("analysis/rdata-files/CatCont_results.RDS")
}

# continue samples
run_more = F # set to T to run more samples

if (run_more){
  for (g in names(group_res)){
    continued = continueSampling(group_res[[g]], 2500)
    group_res[[g]] = continued$combinedResults
  }
  saveRDS(group_res, file="analysis/rdata-files/CatCont_results.RDS")
}

length(group_res$Younger$posteriors$`pMem[1-101]`)

# save/load  
# saveRDS(group_res, file="analysis/CatCont_results.RDS")
# group_res = readRDS("analysis/CatCont_results.RDS")

# combine
cc_res = combineGroupResults.BP(group_res)

# remove burn-in 
cc_res = removeBurnIn(cc_res, 1000)

# convergence
assess_conv = F

if (assess_conv){
  library(coda)
  pmat = convertPosteriorsToMatrix(cc_res) 
  pmat = mcmc(pmat)
  
  gr = geweke.diag(pmat, 0.5, 0.5)
  #par(mar=c(4, 4, 1, 1))
  qqnorm(gr$z, main="") # The z-scores should follow a standard normal distribution. 
  abline(0,1)
}

# plot parameter estimates
plotParameterLineChart(cc_res, "pMem", cip = .68)
mtext("Probability the probed item is in memory", font=2, adj = 0)

plotParameterLineChart(cc_res, "pContBetween", cip = .68)
mtext("Probability the information in memory is continuous vs categorical", font=2, adj = 0)

plotCatMu(cc_res)
mtext("Category locations", font=2, adj = 0)

plotParameterLineChart(cc_res, "contSD", cip = .68)
mtext("Precision of continuous information in memory", font=2, adj = 0)

posteriorPredictivePlot(cc_res$groups$Younger)
posteriorPredictivePlot(cc_res$groups$Older)

# extract param samples
cc_post_pm = getConditionEffects(cc_res, param = "pMem", addMu = T, manifest = T)$post
cc_post_pc = getConditionEffects(cc_res, param = "pContBetween", addMu = T, manifest = T)$post
cc_post_sd = getConditionEffects(cc_res, param = "contSD", addMu = T, manifest = T)$post

apply(cc_post_pm[,1:3] - cc_post_pm[,4:6], 1, mean)
apply(cc_post_pc[,1:3] - cc_post_pc[,4:6], 1, mean)
apply(cc_post_sd[,1:3] - cc_post_sd[,4:6], 1, mean)
