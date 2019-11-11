
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Analysis code for
# Age differences in the precision of memory at short and long delays
# Rhodes, Abbene, Meierhofer, & Naveh-Benjamin
# 
# Code to read in the data from Experiment 1
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

library(plyr)

setwd("data/exp1/")

data_files = list.files(pattern = 'precision')

prec_dat = data.frame()
for (f in data_files){
  tmp = read.csv(f)
  if (nrow(tmp) != 108){
    print(paste(f))
  }
  info = strsplit(f, '_')[[1]]
  tmp$pNum = as.numeric(gsub(pattern = 'p', replacement = '', x = info[2]))
  tmp$group = as.numeric(gsub(pattern = 'g', replacement = '', x = info[3]))
  prec_dat=rbind(prec_dat, tmp)
}

# give participants a unique ID
prec_dat$ID = prec_dat$group*100 + prec_dat$pNum

# add a delay factor
prec_dat$delay=NA
prec_dat$delay[prec_dat$lag %in% c(0,1,2)]<-'short'
prec_dat$delay[prec_dat$lag %in% c(10,11,12)]<-'medium'
prec_dat$delay[prec_dat$lag %in% c(23,24,25)]<-'long'
prec_dat$delay=factor(prec_dat$delay, levels = c('short', 'medium', 'long'))

# figure out what responses are missing
prec_dat$recallMissed = as.integer(is.na(prec_dat$recallRT))
prec_dat$confMissed = as.integer(is.na(prec_dat$confRT))
prec_dat$notMoved = as.integer(with(prec_dat, sqrt(recalledX^2 + recalledY^2)) < 100*sqrt(2))
prec_dat$outBounds = as.integer(with(prec_dat, sqrt(recalledX^2 + recalledY^2)) > 500-100*sqrt(2)/2)
# these are fully complete observations
prec_dat$fullObs = as.integer(!with(prec_dat, recallMissed == 1 | notMoved == 1 | outBounds == 1))

# what proportion complete for each participant
complete_by_ID = aggregate(fullObs~ID, data = prec_dat, FUN = function(x) round(mean(x)*100, 2))

plot(complete_by_ID)

# participants with > 25% missing data are removed
to_remove = subset(complete_by_ID, fullObs < 75)$ID

# copy over
prec_dat_full = prec_dat

prec_dat = subset(prec_dat, !(ID %in% to_remove))

mean(prec_dat$fullObs)
mean(prec_dat$recallMissed)

# write 'cleaned' file
write.csv(prec_dat, file = "exp1_cleaned.csv", row.names = F)

setwd("../..")

