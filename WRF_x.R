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
library("dplyr")
library("tidyverse")
library("data.table")
library("lubridate")
library("parallel")

calculate_mean_previous_year <- function(df, i){
  
  mean_prev_year <- NA
  
  id <- df$PatientID[i]
  date <- df$event.date[i]
  
  df_tmp <- df %>%
    filter(PatientID == id) %>%
      mutate(tmp_date = date,
             tmp_date_365_before = date - 365) %>%
        filter(event.date < date & event.date >= tmp_date_365_before)
  
  if(nrow(df_tmp) > 0){
    
    mean_prev_year <- mean(df_tmp$Creatinine, na.rm = TRUE)
    
  }
 
  mean_prev_year
  
}

#define number of cores

cores_number <- 60

crea.rep$WRF365Mean_x <- unlist(mclapply(X = 1:nrow(crea.rep),
                                         FUN = calculate_mean_previous_year,
                                         df = crea.rep,
                                         mc.cores = cores_number, 
                                         mc.silent = FALSE))

crea.rep <- crea.rep %>% 
              mutate(WRFcreaDiff_x = ifelse(!is.na(WRF365Mean_x), # only if consecutive years
                                            Creatinine-WRF365Mean_x,
                                            NA))


## categorize WRF
crea.rep$WRFx <- as.numeric(0)

crea.rep$WRFx <- ifelse(crea.rep$WRFcreaDiff_x >= 17.7 & crea.rep$WRFcreaDiff_x <= 26.5,
                       1,
                       crea.rep$WRFx)

crea.rep$WRFx <- ifelse(crea.rep$WRFcreaDiff_x > 26.5 & crea.rep$WRFcreaDiff_x <=44.2,
                       2,
                       crea.rep$WRFx)

crea.rep$WRFx <- ifelse(crea.rep$WRFcreaDiff_x > 44.2,
                       3,
                       crea.rep$WRFx)

crea.rep$WRFx <- ifelse(is.na(crea.rep$WRFcreaDiff_x),
                       NA_integer_,
                       crea.rep$WRFx)

head(crea.rep)

#count different categories
crea.rep %>%
  group_by(WRFx) %>%
  count()

# A tibble: 5 x 2
# Groups:   WRFx [5]
# WRFx      n
# <dbl>  <int>
# 1     0 239681
# 2     1  13516
# 3     2  11560
# 4     3  13991
# 5    NA  23336


