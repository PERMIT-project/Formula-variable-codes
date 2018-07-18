library("dplyr")
library("tidyverse")
library("stringr")

load("SIR_crea.select.rda")

calculate_mean_sd <- function(col_name, df, digits = 1){
  
  m <- df[[col_name]] %>%
        mean() %>%
          round(digits = digits)
  
  sd <- df[[col_name]] %>%
          sd() %>%
            round(digits = digits)
  
  str_c(m, " (", sd, ")", sep = "")
  
}

calculate_n_prev <- function(col_name, df, digits = 1){
  
  colnames(df)[colnames(df) == col_name] <- "outcome"
  
  df <- df %>%
          group_by(PatientID) %>%
            filter(event.date == max(event.date))
              
  if(is.numeric(df$outcome)){
    
    results <- data.frame(Characteristic = col_name, 
                         Value = str_c(format(sum(df$outcome), big.mark = ","),
                                       " (",
                                       round(sum(df$outcome)/nrow(df)*100, digits = digits),
                                       ")",
                                       sep = ""))
    
  }else{
    
    results <- df %>%
                group_by(outcome) %>%
                  count() %>%
                    ungroup() %>%
                      mutate(Value = str_c(format(n, big.mark = ","), 
                                           " (",
                                           round(n/nrow(df)*100, digits = digits),
                                           ")",
                                           sep = "")) %>%
                        rename(Characteristic = outcome) %>%
                          select(-n) %>%
                            mutate(Characteristic = str_c(col_name,
                                                          " - ",
                                                          Characteristic,
                                                          sep = ""))
    
  }
  
  results <- results %>%
    mutate(Characteristic = str_c(Characteristic,
                                  " (%)",
                                  sep = ""))
  
  results
    
}


calculate_n_prev_max <- function(col_name, df, digits = 1){
  
  colnames(df)[colnames(df) == col_name] <- "outcome"
  
  df <- df %>%
    group_by(PatientID) %>%
      filter(outcome == max(outcome, na.rm = TRUE)) %>%
        ungroup() %>%
          distinct(PatientID, outcome)
  
  if(is.numeric(df$outcome)){
    
    results <- data.frame(Characteristic = col_name, 
                          Value = str_c(format(sum(df$outcome), big.mark = ","),
                                        " (",
                                        round(sum(df$outcome)/nrow(df)*100, digits = digits),
                                        ")",
                                        sep = ""))
    
  }else {
    
    results <- df %>%
      group_by(outcome) %>%
      count() %>%
      ungroup() %>%
      mutate(Value = str_c(format(n, big.mark = ","), 
                           " (",
                           round(n/nrow(df)*100, digits = digits),
                           ")",
                           sep = "")) %>%
      rename(Characteristic = outcome) %>%
        select(-n) %>%
          mutate(Characteristic = str_c(col_name,
                                        " - ",
                                        Characteristic,
                                        sep = ""))
    
  }
  
  results <- results %>%
    mutate(Characteristic = str_c(Characteristic,
                                  " (%)",
                                  sep = ""))
  
  results
  
}


calculate_mean_sd_lab_test <- function(col_name, df, digits = 1){
  
  colnames(df)[colnames(df) == col_name] <- "outcome"
  
  pat_N <- length(unique(df$PatientID))
  
  df <- df %>%
    filter(!is.na(outcome)) %>%
    group_by(PatientID) %>%
      summarise(pat_mean = mean(outcome, na.rm = TRUE)) %>%
        ungroup()
  
  pat_with_missing <- pat_N - length(unique(df$PatientID))
  
  results <- data.frame(Characteristic = str_c("Mean ",
                                               col_name,
                                               " (sd)",
                                               sep = ""), 
                        Value = str_c(round(mean(df$pat_mean, na.rm = TRUE), digits),
                                      " (",
                                      round(sd(df$pat_mean, na.rm = TRUE), digits),
                                      ")",
                                      sep = ""),
                        'Patients without any value (%)' = str_c(format(pat_with_missing, big.mark = ","),
                                                                 " (",
                                                                 round(pat_with_missing/pat_N*100, digits = digits),
                                                                 ")",
                                                                 sep = ""))
  
  results

}

# 
# IDs <- crea.rep %>%
#         filter(event.date >= as.Date("2008-01-01") & TimeSincehf >= 0) %>%
#           distinct(PatientID)
# 
# 
# save(crea.rep, file = "SIR_crea.select_refined.rda")

crea.rep <- crea.rep %>%
  #filter(PatientID %in% IDs$PatientID) %>%
    mutate(Ethnicity = factor(Ethnicity))


length(unique(crea.rep$PatientID))
#[1] 6067

dataset_summary <- data.frame(Characteristic = "Number of patients",
                              Value = format(length(unique(crea.rep$PatientID)), big.mark = ",")) %>%
                   bind_rows(data.frame(Characteristic = "Mean age at baseline (sd)",
                                        Value = calculate_mean_sd(col_name = "Age", #Age at baseline (e.g. first hf code)
                                                                  df = crea.rep %>% 
                                                                    group_by(PatientID) %>% 
                                                                    filter(TimeSincehf >=0) %>% 
                                                                    filter(TimeSincehf == min(TimeSincehf))))) %>%
                    bind_rows(data.frame(Characteristic = "Max age (sd)",
                                         Value = calculate_mean_sd(col_name = "Age", #Max age reached
                                                                   df = crea.rep %>% 
                                                                     group_by(PatientID) %>% 
                                                                     filter(Age == max(Age))))) %>%
                    bind_rows(data.frame(Characteristic = "HF age (sd)",
                                         Value = calculate_mean_sd(col_name = "hfage", #Max age reached
                                                                   df = crea.rep %>% 
                                                                          distinct(PatientID, hfage)))) %>%
                    bind_rows(data.frame(Characteristic = "Follow-up in years (sd)",
                                         Value = calculate_mean_sd(col_name = "TimeSincehf", #Max age reached
                                                                   df = crea.rep %>% 
                                                                     group_by(PatientID) %>% 
                                                                     filter(TimeSincehf == max(TimeSincehf)) %>% 
                                                                      mutate(TimeSincehf = TimeSincehf/365))))

CKD_list <- c("2", "3a", "3b", "4", "5")                   

CKDstage_columns <- c("CKDStage_MDRDeGFR", "CKDStage_CKDEPIeGFR")

for(j in 1:length(CKDstage_columns)){
  
  crea.rep$CKDStage <- crea.rep[[CKDstage_columns[j]]]
  
  for(i in 1:length(CKD_list)){
    
    tmp <- crea.rep %>%
      filter(CKDStage %in% CKD_list[i:length(CKD_list)]) %>%
        distinct(PatientID)
    
    dataset_summary <- dataset_summary %>%
      bind_rows(data.frame(Characteristic = str_c(CKDstage_columns[j],
                                                   " - At least ",
                                                  CKD_list[i],
                                                  " (%)"),
                           Value = str_c(format(nrow(tmp), big.mark = ","),
                                         " (",
                                         round(nrow(tmp)/length(unique(crea.rep$PatientID))*100, digits = 1),
                                         ")",
                                         sep = "")))
    
  }
  
}

### AKI
tmp <- crea.rep %>% 
         distinct(PatientID, NoAKIepisodes) %>%
            group_by(PatientID) %>% 
              filter(NoAKIepisodes == max(NoAKIepisodes)) %>% 
                ungroup() 


dataset_summary <- dataset_summary %>%
                      bind_rows(data.frame(Characteristic = "Mean number of AKI episodes (sd)",
                                           Value = str_c(round(mean(tmp$NoAKIepisodes), 1),
                                                         " (",
                                                         round(sd(tmp$NoAKIepisodes), 1),
                                                         ")",
                                                         sep = "")))






logical_values <- c("Gender",
                    "Death",
                    "Ethnicity",
                    #"CKDStage_MDRDeGFR",
                    #"CKDStage_CKDEPIeGFR",
                    "Nephrectomy",
                    "RRT",
                    'Diabetes',
                    'AF',
                    'IHD',
                    'PVD',
                    'RenalMalignancy',
                    'RenalTransplant',
                    'CLD')


for(i in 1:length(logical_values)){
  
  dataset_summary <- dataset_summary %>%
                       bind_rows(calculate_n_prev(col_name = logical_values[i], df = crea.rep))    
  
}

logical_values <- c("Smoker",
                    "Anaemia",
                    "Antimicrobial_yn",
                    "ARBs_yn",
                    "DIUR_THI_yn",
                    "Immunosuppressants_yn",
                    "LoopDiuretics_yn",
                    "NSAID_yn",
                    "Othernephrotoxins_yn")


for(i in 1:length(logical_values)){
  
  dataset_summary <- dataset_summary %>%
    bind_rows(calculate_n_prev_max(col_name = logical_values[i], df = crea.rep))    
  
}


numerical_values <- c("Creatinine",
                      "MDRDeGFR",
                      "CKDEPIeGFR",
                      "SerumSodium",
                      "BMI",
                      "UricAcid",
                      "BUN",
                      "SerPotassium",
                      "HeartRate",
                      "BNP",
                      "NTPROBNP",
                      "SBP",
                      "DBP",
                      "SerumAlbumin",
                      "UrineAlbumin",
                      "UACratio",
                      "MCV",
                      "Haemoglobin")

for(i in 1:length(numerical_values)){
  
  dataset_summary <- dataset_summary %>%
    bind_rows(calculate_mean_sd_lab_test(col_name = numerical_values[i], df = crea.rep))    
  
}

write.csv(dataset_summary, "SIR_summary_statistics.csv", row.names =  FALSE)
