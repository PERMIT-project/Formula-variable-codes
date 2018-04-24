
install.packages("data.table")
# if there should be any problems with data.table installation, please refer to:
# https://github.com/Rdatatable/data.table/wiki/Installation
library("data.table")
require("dplyr")
cessationCodes <- read.csv("cessation1.csv")
smok <- read.csv("smoking1.csv")
sir.smok <- sir.data
sir.smok$Smoker <- ifelse( sir.smok$ReadCode %in% smok$ReadCode, 1, 0)

## loop through all patients, if they have at least one "1" in "Smokers", the sum(1 %in% x) will be different
## than 0, i.e. that patient will get FALSE. In that way I can select all the patients from SIR data which have at least
## one smoking ReadCode and save all the data for certain patient (i.e. even the rows with 0s, those are the rows which do not have a smoking
## ReadCode, but since this patient has at least one row wirh smoking ReadCode, that means that this patient had been/has been a smoker)
ifsmoker <- tapply(sir.smok$Smoker, sir.smok$PatientID, function(x) sum(1 %in% x)==0)
ifsmoker <- ifsmoker[ifsmoker==FALSE]
ifsmoker <- as.numeric(names(ifsmoker))

## do the same with cessation readcodes.
sir.smok$cessation <- ifelse( sir.smok$ReadCode %in% cessationCodes$ReadCode, 1, 0)
setDT(sir.smok)
year <- substr(sir.smok$EntryDate,1,4)
month <- substr(sir.smok$EntryDate,5,6)
day <- substr(sir.smok$EntryDate, 7,8)
as.Date(paste(year,month,day,sep="-")) -> sir.smok$event.date
setkey(sir.smok, PatientID)
sir.smok <- sir.smok[!(cessation == 0 & event.date < "2008-01-01")] # to remove all stop codes before Jan 1, 2008
sir.smok <- sir.smok[Smoker==1 | cessation== 1]# to select rows which have either start or end signal. Rows with zeroes in both columns will be discarded
save(sir.smok,file="~/Documents/PERMIT/smoking/sir.smok.rda")

# cessationCodes <- read.csv("~/Documents/PERMIT/newVariable/Reviewed_code_lists/cessation.csv")
# smok <- read.csv("~/Documents/PERMIT/newVariable/Reviewed_code_lists/smoking.csv")
load("crea.rep013118_egfr.rda")
crea <- crea.rep
load("smoking/sir.smok.rda")

# sir.smok.rda data:
  # all stop codes before Jan 1, 2008 were removed
  # all rows with 0 in both Smoking and cessation column were removed

### FIND THE MATCH BETWEEN SMOKING DATA AND CREATININE DATA
## USING DATA.TABLE FUNCTION ROLL
## WHEN ROLL=T, THE FUNCTION WILL SEARCH FOR A SMOKING EVENT.DATE WHICH PRECEDED CREATININE EVENT.DATE
## and then add all the columns from selected smoking date to that creatinine date

## PRIOR TO ROLLING IT IS IMPORTANT TO:
##- set both data sets as data tables
##- setkey-s for both data tables to PatientID and then event.date (grouos in the background)
##- for each data set, duplicate event date column and rename it specifically. eg crea_eventDate and smok_eventdate
    ## to note which event date belongs to which data after matching


# select only columns we need for rolling:
setDT(sir.smok)
setDT(crea)
sir.smok2 <- sir.smok[,c("PatientID", "event.date", "Smoker", "cessation")]
# add smokeventdate column so that I know which event dates belong to smoking after rolling
sir.smok2 <- sir.smok2[, smokeventdate:=event.date]
crea <- crea[, creaeventdate:=event.date] # event.date has a form "2010-01-01"

# order data per patID, then per eventdate and then per ascending Smoker (i.e. so that rows with smoker==1 come first)
sir.smok_unique <- unique(sir.smok2) # to remove duplicated rows
sir.smok_unique <- sir.smok_unique[order(PatientID, event.date, -Smoker)]
#set the keys
setkey(crea, PatientID, event.date)
setkey(sir.smok_unique, PatientID, event.date)

smokToMerge <- sir.smok_unique[,.SD[which.max(Smoker)], by=key(sir.smok_unique)]
# one Smoking marker - e.g. Pat1, event.date 2008-09-23 has both 1 and 0 in the Smoker column. We want to keep the 1
# duplicated function will keep the value which comes first, so it will keep the row with Pat 1, event date 2008-09-23, Smoker==1
# only if we order the column in a descending order. Then the row with Pat 1, event date 2008-09-23, Smoker==0
# (that is why we orderd the data frame -Smoker) will be removed.

#--OR--#
see <- sir.smok2 %>% group_by(PatientID) %>% filter(!duplicated(event.date))
dim(see)==dim(smokToMerge)
#------#
## roll it with roll=T, this will find event.date in
setkey(smokToMerge, PatientID, smokeventdate)

finallymatched <- smokToMerge[crea, roll=T]
finallymatched$event.date <- NULL
setnames(finallymatched, "creaeventdate", 'event.date')
crea.rep <- finallymatched
crea.rep$Smoker[is.na(crea.rep$Smoker)] <- 0
crea.rep$smokeventdate <- NULL
crea.rep$cessation <- NULL
