library("dplyr")
library("tidyverse")
library("data.table")
library("survival")
library("lubridate")

#### /// Worsening Renal Function analysis (WRF)
# each year's creatinine value is subtracted from the previous year's mean
# for the earliest year, i.e., 2008, there is no previous year, so those rows will have NAs
# for, e.g. year 2010, each creatinine value will be subtracted from 2009's mean

# the file is in filesREcreav5_JanFeb2018/
load("SIR_crea.repongoing_with_CKD_diagnosis.rda")
# library("data.table")
crea <- crea.rep
setDT(crea)
setkey(crea, PatientID)
crea <- crea[,eventYear:=substr(EntryDate,1,4)] # extract year for each date

setkey(crea, PatientID, eventYear)
# calculate the year mean per year for each patient
crea <- crea[,YearMean := mean(Creatinine), by=key(crea)]

# do unique per patient's eventYear and YearMean so each patient has 1 mean per year
forMerging <- unique(crea[,.(PatientID, eventYear, YearMean)])

# then shift it altoghether: in that way the earliest year of patient will have NA in the YearMeanShifted
# we need this so that we could easily subtract two columns: a baseline column with a mean of previous year (YearMeanShifted)
# and the current creatinine values
forMerging[, YearMeanShifted := shift(YearMean), by=.(PatientID)]
forMerging[, eventYearShifted := shift(eventYear), by=.(PatientID)]

# this will, for each date in the current year (e.g. 2013), add values of the mean for previous year (2012)
#crea[forMerging, on=.(PatientID, eventYear, YearMean), YearMeanShifted :=i.YearMeanShifted, by=.EACHI]
# # subtract each creatinine value from the baseline (per patient and per eventYear)
# crea[,CreaDiff:=(Creatinine-YearMeanShifted), by=key(crea)]
# setnames(crea, "YearMeanShifted", "WRFBaseline")
# setnames(crea, "YearMean", "WRFyearmean")
# setnames(crea, "CreaDiff", "WRFcreaDiff")

crea <- crea %>%
          as.data.frame() %>%
            left_join(forMerging, by = c("PatientID", "eventYear", "YearMean"))  %>%   
              mutate(WRFcreaDiff = ifelse(!is.na(eventYearShifted) &
                                            ((as.numeric(eventYear) - as.numeric(eventYearShifted)) == 1), # only if consecutive years
                                          Creatinine-YearMeanShifted,
                                          NA)) %>%
                 rename(WRFBaseline = YearMeanShifted,
                        WRFyearmean = YearMean)      

head(crea)


crea.rep <- as.data.frame(crea.rep)

## categorize WRF
crea.rep$WRF <- as.numeric(0)

crea.rep$WRF <- ifelse(crea$WRFcreaDiff >= 17.7 & crea$WRFcreaDiff <= 26.5,
                      1,
                      crea.rep$WRF)

crea.rep$WRF <- ifelse(crea$WRFcreaDiff > 26.5 & crea$WRFcreaDiff <=44.2,
                       2,
                       crea.rep$WRF)

crea.rep$WRF <- ifelse(crea$WRFcreaDiff > 44.2,
                       3,
                       crea.rep$WRF)

crea.rep$WRF <- ifelse(is.na(crea$WRFcreaDiff),
                       NA_integer_,
                       crea.rep$WRF)

head(crea.rep)

#count different categories
crea.rep %>%
  group_by(WRF) %>%
    count()
# A tibble: 5 x 2
# Groups:   WRF [5]
# WRF      n
# <dbl>  <int>
# 1     0 214863
# 2     1  15724
# 3     2  15450
# 4     3  21551
# 5    NA  34496
