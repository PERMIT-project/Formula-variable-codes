# date()
# install.packages("data.table")
# # if there should be any problems with data.table installation, please refer to:
# # https://github.com/Rdatatable/data.table/wiki/Installation
library("data.table")
require("dplyr")
library("stringr")
library("parallel")

load("SIR_crea.repongoing_with_WRF.rda")

calculate_AnnualPerc <- function(df, i, outcome_name, covariates_name, number_of_years = 4){
  
  #change name to outcome variable
  colnames(df)[colnames(df) == outcome_name] <- "Outcome"
  
  # initialise variables
  id <- df$PatientID[i]
  date <- df$event.date[i]
  number_of_days <- 365*number_of_years 
  coef <- NA
  perc <- NA
  
  # select relevant data
  df_tmp <- df %>%
              filter(PatientID == id) %>% # only data for ith patient
                mutate(tmp_date = date,
                       tmp_date_before = tmp_date - number_of_days) %>%
                  filter(event.date <= tmp_date & event.date >= tmp_date_before) # only data within the last X number of days
  
  if(uniqueN(year(df_tmp$event.date)) >= (number_of_years + 1)){ # only if there is data for each of the last X years
    
    # dinamically create formula
    formula_tmp <- as.formula(str_c("Outcome ~",
                                    ifelse(length(covariates_name>1),
                                           str_c(covariates_name, collapse = " + "),
                                           covariates_name)))
    
    coef <- round(as.numeric(coef(lm(formula = formula_tmp,
                                     data=df_tmp))[2]), digits = 3) #regress 4 full years of 
    
    perc <- round(coef/df_tmp[which.min(df_tmp$event.date), "Outcome"], digits = 5) # this is for annual percentage change. the slope is divided by the lowest value in the particular 4 year period
      
  }
  
  # create result df
  result <- data.frame(PatientID = id,
                       event.date = date,
                       RRD = coef,
                       AnnualPerc = perc)
  
  variable_names <- c("RRD", "AnnualPerc")
  
  #change names for specific outcome
  colnames(result)[colnames(result) %in% variable_names] <- str_c(variable_names,
                                                                  outcome_name,
                                                                  sep = "_")
  
  result
  
}

#initialise number of cores
cores_number <- 60

# calculate annual perc for creatinine
tmp_crea <- rbindlist(mclapply(X = 1:nrow(crea.rep),
                               FUN = calculate_AnnualPerc,
                               df = crea.rep,
                               outcome_name = "Creatinine",
                               covariates_name = "Age",
                               mc.cores = cores_number, 
                               mc.silent = FALSE))

# calculate annual perc for CKDEPI
tmp_CKDEPI <- rbindlist(mclapply(X = 1:nrow(crea.rep),
                               FUN = calculate_AnnualPerc,
                               df = crea.rep,
                               outcome_name = "CKDEPIeGFR",
                               covariates_name = "Age",
                               mc.cores = cores_number, 
                               mc.silent = FALSE))
  
# calculate annual perc for MDRD
tmp_MDRD <- rbindlist(mclapply(X = 1:nrow(crea.rep),
                                 FUN = calculate_AnnualPerc,
                                 df = crea.rep,
                                 outcome_name = "MDRDeGFR",
                                 covariates_name = "Age",
                                 mc.cores = cores_number, 
                                 mc.silent = FALSE))

# join to crea.rep
crea.rep <- crea.rep %>%
  left_join(tmp_crea, by = c("PatientID", "event.date")) %>%
    left_join(tmp_CKDEPI, by = c("PatientID", "event.date")) %>%
      left_join(tmp_MDRD, by = c("PatientID", "event.date"))


head(crea.rep)
