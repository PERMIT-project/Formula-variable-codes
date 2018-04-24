######!! eGFR
crea.rep$CKDEPIeGFR <- NA

## CKD-EPI equation
## if ethnicity is 4, then the person is African American. Person is considered 'white' otherwise.
crea.rep$CKDEPIeGFR<- ifelse(is.na(crea.rep$CKDEPIeGFR) & crea.rep$Gender=="M" & crea.rep$Ethnicity==4, (141*(pmin((crea.rep$Creatinine/79.6),1)^(-0.411))*(pmax((crea.rep$Creatinine/79.6), 1)^(-1.209))*(0.993^(crea.rep$Age))*1.159),  crea.rep$CKDEPIeGFR)
crea.rep$CKDEPIeGFR <- ifelse(is.na(crea.rep$CKDEPIeGFR) & crea.rep$Gender=="M"& crea.rep$Ethnicity!=4, (141*(pmin((crea.rep$Creatinine/79.6),1)^(-0.411))*(pmax((crea.rep$Creatinine/79.6), 1)^(-1.209))*(0.993^(crea.rep$Age))),  crea.rep$CKDEPIeGFR)
crea.rep$CKDEPIeGFR <- ifelse(is.na(crea.rep$CKDEPIeGFR) & crea.rep$Gender=="F" & crea.rep$Ethnicity==4, (141*(pmin((crea.rep$Creatinine/61.9),1)^(-0.329))*(pmax((crea.rep$Creatinine/61.9), 1)^(-1.209))*(0.993^(crea.rep$Age))*1.018*1.159),  crea.rep$CKDEPIeGFR)
crea.rep$CKDEPIeGFR <- ifelse(is.na(crea.rep$CKDEPIeGFR), (141*(pmin((crea.rep$Creatinine/61.9),1)^(-0.329))*(pmax((crea.rep$Creatinine/61.9), 1)^(-1.209))*(0.993^(crea.rep$Age))*1.018), crea.rep$CKDEPIeGFR)

sum(is.na(crea.rep$CKDEPIeGFR)) # has to be 0
# compare with web calculator:
#https://www.niddk.nih.gov/health-information/health-communication-programs/nkdep/lab-evaluation/gfr-calculators/adults-si-unit-ckd-epi/Pages/default.aspx
