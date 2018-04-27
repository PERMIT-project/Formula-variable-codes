#### /// WRFx - Worsening Renal Function X analysis
# for each creatinine value, take values one year in past, calculate that year's mean
# and subtract current creatinine value from the mean
# this is different from previous WRF in a way that each creatinine value has a different
# mean from which it is subtracted
#-------------------------------------------
##!!!! MORE NAs IN WRFx THAN IN WRFcreaDiff
##// here it can happen that some dates within the patient have more NAs for WRFx than they have for WRF
##// that is because if we take some date to calculate WRF, it will probably have a year Mean from the whole last year to subtract it from (it just looks at the previous year, say whole 2015, no matter the date in 2016 that is our current date)
##// for WRFx, it can happen that for some date, one year in the past (exactly 365 days ago) there is no data at all, and that row will have NA
# e.g. Pat 21246, date 2016-07-18 has no data in the last 365 days because the nearest date in past
# is more than 365 days away - 2015-03-17. It will have WRFcreaDiff calculated because it will take 2015's mean and subtract
#-------------------------------------------

crea.rep <- yourdata
setDT(crea.rep)
setorder(crea.rep, PatientID, -event.date)
crea.rep[, WRFcreaDiff_x := as.numeric(NA)]
crea.rep[, WRF365Mean_x := as.numeric(NA)]

print("lapply started")
lapply(unique(crea.rep$PatientID), function(x)
{
  rows <- which(crea.rep$PatientID %in% x)
  data.x <- crea.rep[PatientID == x]
  print(paste0("patient ID is ", x))

  for(i in 1:nrow(data.x))
  {
    # theDate <- as.Date("2015-07-10")
    theDate <- data.x[,event.date][i]

    # x <- "21246"
    # theDate <- as.Date("2016-07-18")
    if(theDate >= "2008-12-31") ##dates before 31.12.2008 will not have 1 year of data (theDate >= "2008-12-31" or theDate < "2011-12-31")???
    {
      d <- theDate - 365
      idata <- data.x[data.x$event.date < theDate & data.x$event.date >= d,]
      creaRow <- rows[i]
      wrf_mean <- mean(idata$Creatinine)

      crea.rep[creaRow, WRFcreaDiff_x := as.numeric(Creatinine-wrf_mean)]
      crea.rep[creaRow, WRF365Mean_x := as.numeric(wrf_mean)]
    }
  }
  print('lapply done')
})


#// add WRF categories
crea.rep[, WRFx := as.numeric(0)]
crea.rep[,WRFx:=ifelse(WRFcreaDiff_x >= 17.7 & WRFcreaDiff_x <= 26.5 & crea.rep$WRFx==0, 1, crea.rep$WRFx)]
crea.rep[,WRFx:=ifelse(WRFcreaDiff_x > 26.5 & WRFcreaDiff_x <=44.2, 2, crea.rep$WRFx)]
crea.rep[,WRFx:=ifelse(WRFcreaDiff_x > 44.2, 3, crea.rep$WRFx)]
crea.rep[crea.rep[,is.na(crea.rep$WRFx)], "WRFx"] <- 0
