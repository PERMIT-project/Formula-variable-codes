date()
install.packages("data.table")
# if there should be any problems with data.table installation, please refer to:
# https://github.com/Rdatatable/data.table/wiki/Installation
library("data.table")
require("dplyr")


## set up your data set
crea.t <- yourdata
setDT(crea.t)
setorder(crea.t, PatientID, -event.date)
crea.t[,event.date := as.Date.character(event.date)]

####/// RRD using Creatinine
crea.t[,RRD_crea:=as.numeric(NA)]
crea.t[,AnnualPerc_crea:=as.numeric(NA)]
crea.t[,eventYear:=substr(event.date,1,4)]

print("lapply started RRD crea")

lapply(unique(crea.t$PatientID), function(x)
{

  rows <- which(crea.t$PatientID %in% x)
  data.x <- crea.t[PatientID == x]
  print(paste0("patient ID is ", x))

  for(i in 1:nrow(data.x))
  {
    theDate <- data.x[,event.date][i]
    if(theDate >= "2011-12-31")  ##dates before 31.12.2011 will not have 4 years of data
    {
      d <- as.Date(theDate) - 1460
      idata <- data.x[data.x$event.date <= theDate & data.x$event.date>= d,]
      creaRow <- rows[i]

      if(uniqueN(idata$eventYear) >= 5) # if uniqueN < 5 that means that we only have data in 3, e.g. the year we are looking at (2016) and the 4 years before (2012), but no data from years between (2013,2014, 2015)
      {
        coef <- round(as.numeric(coef(lm(Creatinine ~  Age, data=idata))[2]), digits = 3) #regress 4 full years of creatinine data on Age (age of the patient at the time of creatinine measurement); take the slope
        crea.t[creaRow, RRD_crea:=coef]
        perc <- round(coef/idata[which.min(event.date), Creatinine], digits = 3) # this is for annual percentage change. the slope is divided by the lowest value in the particular 4 year period
        crea.t[creaRow, AnnualPerc_crea := perc]
      }
    }
  }
})
print("crea function done")
date()


####///// CKDEPI eGFR

setorder(crea.t, PatientID, -event.date)
crea.t[,event.date := as.Date.character(event.date)]
crea.t[,RRD_CKDEPI:=as.numeric(NA)]
crea.t[,AnnualPerc_CKDEPI:=as.numeric(NA)]
print("lapply started RRD CKD EPI")

lapply(unique(crea.t$PatientID), function(x)
{
  rows <- which(crea.t$PatientID %in% x)
  data.x <- crea.t[PatientID == x]
  print(paste0("patient ID is ", x))

  for(i in 1:nrow(data.x))
  {
    theDate <- data.x[,event.date][i]
    if(theDate >= "2011-12-31")  ##dates before 31.12.2011 will not have 4 years of data
    {
      d <- as.Date(theDate) - 1460
      idata <- data.x[data.x$event.date <= theDate & data.x$event.date>= d,]
      creaRow <- rows[i]

      if(uniqueN(idata$eventYear) >= 5) # if uniqueN < 5 that means that we only have data in 3, e.g. the year we are looking at (2016) and the 4 years before (2012), but no data from years between (2013,2014, 2015)
      {
        coef <- round(as.numeric(coef(lm(CKDEPIeGFR ~  Age, data=idata))[2]), digits = 3)
        crea.t[creaRow, RRD_CKDEPI:=coef]
        perc <- round(coef/idata[which.min(event.date), CKDEPIeGFR], digits = 3)
        crea.t[creaRow, AnnualPerc_CKDEPI := perc]
      }
    }
  }
})
print("CKDEPI function done")



####///// MDRD eGFR
crea.t[,RRD_MDRD:=as.numeric(NA)]
crea.t[,AnnualPerc_MDRD:=as.numeric(NA)]

print("lapply started RRD MDRD")

lapply(unique(crea.t$PatientID), function(x)
{
  rows <- which(crea.t$PatientID %in% x)
  data.x <- crea.t[PatientID == x]
  print(paste0("patient ID is ", x))

  for(i in 1:nrow(data.x))
  {
    theDate <- data.x[,event.date][i]
    if(theDate >= "2011-12-31")  ##dates before 31.12.2011 will not have 4 years of data
    {
      d <- as.Date(theDate) - 1460
      idata <- data.x[data.x$event.date <= theDate & data.x$event.date>= d,]
      creaRow <- rows[i]

      if(uniqueN(idata$eventYear) >= 5) # if uniqueN < 5 that means that we only have data in 3, e.g. the year we are looking at (2016) and the 4 years before (2012), but no data from years between (2013,2014, 2015)
      {
        coef <- round(as.numeric(coef(lm(MDRDeGFR ~  Age, data=idata))[2]), digits = 3)
        crea.t[creaRow, RRD_MDRD:=coef]
        perc <- round(coef/idata[which.min(event.date), MDRDeGFR], digits = 3)
        crea.t[creaRow, AnnualPerc_MDRD := perc]
      }
    }
  }
})
print("MDRD function done")
