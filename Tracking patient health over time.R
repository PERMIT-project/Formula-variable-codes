#It can be useful to follow patient progression over time. The following code calls the first recorded metric after a 6 month interval
#where this data is available, and appends this to each row.

###################################################################################
#FIRST LOG CREATININE AFTER 6M

first<-crea.rep[,c("PatientID","event.date","log_CREA")]
first$event.date2<-first$event.date+180
indx1<-neardate(first$PatientID,crea.rep$PatientID,first$event.date2,crea.rep$event.date,best="after",nomatch=NA_integer_)
first$LogCrea6M<-crea.rep[indx1,"log_CREA"]

#THE ABOVE CODE PREFERABLY MATCHES A RESULT FROM AFTER 6 MONTHS, BUT OTHERWISE SELECTS ONE BEFORE IT. 
#WE NEED TO REMOVE DATA COLLECTED FROM BEFORE 6 MONTHS:

maxes<-first %>%
  group_by(PatientID) %>%
  slice(which.max(event.date)) %>%
  as.data.frame
maxes<-maxes[,c(1,2)]
colnames(maxes)<-c("PatientID","MaxDate")
first<-merge(first,maxes)
first<-first[first$event.date2<=first$MaxDate,c(1,2,5)]
first$PatientID<-as.character(first$PatientID)
crea.rep$PatientID<-as.character(crea.rep$PatientID)
crea.rep<-merge(crea.rep,first,all.x=TRUE,all.y=FALSE)
crea.rep$LogCrea6M<-unlist(crea.rep$LogCrea6M)

###################################################################################
#FIRST EGFR AFTER 6M-MDRD

first<-crea.rep[,c("PatientID","event.date","MDRDeGFR")]
first$event.date2<-first$event.date+180

indx1<-neardate(first$PatientID,crea.rep$PatientID,first$event.date2,crea.rep$event.date,best="prior",nomatch=NA_integer_)
first$MDRDeGFR6M<-crea.rep[indx1,"MDRDeGFR"]

#THE ABOVE CODE PREFERABLY MATCHES A RESULT FROM AFTER 6 MONTHS, BUT OTHERWISE SELECTS ONE BEFORE IT. 
#WE NEED TO REMOVE DATA COLLECTED FROM BEFORE 6 MONTHS:

maxes<-first %>%
  group_by(PatientID) %>%
  slice(which.max(event.date)) %>%
  as.data.frame
maxes<-maxes[,c(1,2)]
colnames(maxes)<-c("PatientID","MaxDate")

first$PatientID<-as.character(first$PatientID)
maxes$PatientID<-as.character(maxes$PatientID)
first<-merge(first,maxes)
first<-first[first$event.date2<=first$MaxDate,c(1,2,5)]

crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$MDRDeGFR6M<-unlist(crea.rep$MDRDeGFR6M)

################################################################################### CHECKED
#FIRST CKDEPI AFTER 6M

first<-crea.rep[,c("PatientID","event.date","CKDEPIeGFR")]
first$event.date2<-first$event.date+180

indx1<-neardate(first$PatientID,crea.rep$PatientID,first$event.date2,crea.rep$event.date,best="prior",nomatch=NA_integer_)
first$CKDEPIeGFR6M<-crea.rep[indx1,"CKDEPIeGFR"]

#THE ABOVE CODE PREFERABLY MATCHES A RESULT FROM AFTER 6 MONTHS, BUT OTHERWISE SELECTS ONE BEFORE IT. 
#WE NEED TO REMOVE DATA COLLECTED FROM BEFORE 6 MONTHS:

maxes<-first %>%
  group_by(PatientID) %>%
  slice(which.max(event.date)) %>%
  as.data.frame
maxes<-maxes[,c(1,2)]
colnames(maxes)<-c("PatientID","MaxDate")
first<-merge(first,maxes)
first<-first[first$event.date2<=first$MaxDate,c(1,2,5)]

crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$CKDEPIeGFR6M<-unlist(crea.rep$CKDEPIeGFR6M)

################################################################################### CHECKED

#FIRST CREATININE AFTER 6M
first<-crea.rep[,c("PatientID","event.date","Creatinine")]
first$event.date2<-first$event.date+180

indx1<-neardate(first$PatientID,crea.rep$PatientID,first$event.date2,crea.rep$event.date,best="prior",nomatch=NA_integer_)
first$Creatinine6M<-crea.rep[indx1,"Creatinine"]

#THE ABOVE CODE PREFERABLY MATCHES A RESULT FROM AFTER 6 MONTHS, BUT OTHERWISE SELECTS ONE BEFORE IT. 
#WE NEED TO REMOVE DATA COLLECTED FROM BEFORE 6 MONTHS:

maxes<-first %>%
  group_by(PatientID) %>%
  slice(which.max(event.date)) %>%
  as.data.frame
maxes<-maxes[,c(1,2)]
colnames(maxes)<-c("PatientID","MaxDate")
first<-merge(first,maxes)
first<-first[first$event.date2<=first$MaxDate,c(1,2,5)]

crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$Creatinine6M<-unlist(crea.rep$Creatinine6M)

################################################################################