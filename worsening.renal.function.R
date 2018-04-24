install.packages("data.table")
# if there should be any problems with data.table installation, please refer to:
# https://github.com/Rdatatable/data.table/wiki/Installation
library("data.table")
require("dplyr")

#### /// Worsening Renal Function analysis (WRF)
# each year's creatinine value is subtracted from the previous year's mean
# for the earliest year, i.e., 2008, there is no previous year, so those rows will have NAs
# for, e.g. year 2010, each creatinine value will be subtracted from 2009's mean

# load("~/Documents/PERMIT/crea.rep071117SMOKeGFR.rda")
# load("~/Documents/PERMIT/crea.rep2yrsall.rda")

# the file is in filesREcreav5_JanFeb2018/
load("~/Documents/PERMIT/crea.rep_RRD_SMOKER_eGFR.rda")
# library("data.table")
crea <- crea.rep
setDT(crea)
setkey(crea, PatientID)
crea <- crea[,eventYear:=substr(event.date,1,4)]
setkey(crea, PatientID, eventYear)
# calculate the year mean per year for each patient
crea <- crea[,YearMean := mean(Creatinine), by=key(crea)]

# do unique per patient's eventYear and YearMean so each patient has 1 mean per year
forMerging <- unique(crea[,.(PatientID, eventYear, YearMean)])

# then shift it altoghether: in that way the earliest year of patient will have NA in the YearMeanShifted
# we need this so that we could easily subtract two columns: a baseline column with a mean of previous year (YearMeanShifted)
# and the current creatinine values
forMerging[, YearMeanShifted := shift(YearMean), by=.(PatientID)]

# this will, for each date in the current year (e.g. 2013), add values of the mean for previous year (2012)
crea[forMerging, on=.(PatientID, eventYear, YearMean), YearMeanShifted :=i.YearMeanShifted, by=.EACHI]

# subtract each creatinine value from the baseline (per patient and per eventYear)
crea[,CreaDiff:=(Creatinine-YearMeanShifted), by=key(crea)]
setnames(crea, "YearMeanShifted", "WRFBaseline")
setnames(crea, "YearMean", "WRFyearmean")
setnames(crea, "CreaDiff", "WRFcreaDiff")

## categorize WRF
crea.rep[, WRF := as.numeric(0)]
crea.rep[,WRF:=ifelse(WRFcreaDiff >= 17.7 & WRFcreaDiff <= 26.5 & crea.rep$WRF==0, 1, crea.rep$WRF)]
crea.rep[,WRF:=ifelse(WRFcreaDiff > 26.5 & WRFcreaDiff <=44.2, 2, crea.rep$WRF)]
crea.rep[,WRF:=ifelse(WRFcreaDiff > 44.2, 3, crea.rep$WRF)]
crea.rep[crea.rep[,is.na(crea.rep$WRF)], "WRF"] <- 0
