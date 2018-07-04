library("parallel")
library("dplyr")
library("data.table")

checkAKI_national_algorithm=function(i,df){
  
  # initialise variables
  id <- df$PatientID[i]
  date <- df$event.date[i]
  c1 <- df$Creatinine[i]
  sex <- df$Gender[i]
  
  # keep only relevant data
  df <- df %>%
    filter(PatientID == id) %>%
      mutate(tmp_date_365_before = date - 365) %>%
        filter(event.date < date & event.date >= tmp_date_365_before)
  
  # initialise flag
  flag <- "No result"
  # most recent first
  df <- df %>% 
          arrange(desc(event.date))
  
  # assign gender specific ref range
  ref.up <- ref.low <- NULL
  
  if(sex=="M"){
    
    ref.low <- 62
    ref.up <- 115
    
  } else {
    
    ref.low <- 44
    ref.up <- 97
    
  }
  # if there is df in one year
  if(nrow(df)>0){
    
    # calculate difference between values
    df$Diff <- as.numeric(date-df$event.date)
    
    # if results in the last 7 days
    rv1.diff <- rv1 <- NULL
    
    if( sum(df$Diff<=7) > 0 ){
      
      # min value in the last 7 days
      tmp <- df %>% 
              filter(Diff <= 7)
      
      rv1 <- min(tmp$Creatinine)
      
      rv1.diff <- tmp %>% 
                    filter(Creatinine == min(Creatinine)) %>% 
                      select(Diff) %>%
                        unlist() %>%
                          unname()
      
    } else {
      # median in the last year
      rv1 <- median(df$Creatinine)
      
      rv1.diff <- 8
      
    }
    # calculate ratio
    rv <- c1/rv1
    
    # check ratio
    if(rv>=1.5){
      
      if(c1>354 | rv>=3){
        
        flag <- "AKI3"
        
      } else if(rv>=2 & rv<3){
        
        flag <- "AKI2"
        
      } else {
        
        flag <- "AKI1"
        
      }
      
    } else{
      
      if(rv1.diff<=2){
        
        if((c1-rv1)>26){
          
          flag <- "AKI1"
          
        } else {
          
          flag <- "No alert"
          
        }
        
      } else{
        
        if((c1-rv1)>26){
          
          flag <- "Increase 26 no alert"
          
        }
        
      }
      
    }
    
  } else{
    
    if(between(x = c1,lower = ref.low,upper = ref.up)){
      
      flag <- "Normal"
      
    } else if(c1<ref.low){
      
      flag <- "Low"
      
    } else{
      
      flag <- "High"
      
    }
    
  }
  
  flag
  
}

count_previous_AKIs <- function(i, df, aki_type = "AKI"){
  
  # initialise variables
  id <- df$PatientID[i]
  date <- df$event.date[i]
  
  
  # keep only relevant data
  df %>%
    filter(PatientID == id & event.date <= date) %>%
      filter(grepl(pattern = str_c("^", aki_type), AKIFlag)) %>%
        nrow()
  
}


cores_number <- 60

# for each creatinine use NHS AKI algorithm
crea.rep$AKIFlag <- unlist(mclapply(X = 1:nrow(crea.rep),
                                    FUN = checkAKI_national_algorithm,
                                    df = crea.rep,
                                    mc.cores = cores_number, 
                                    mc.silent = FALSE))

save(crea.rep, file = "SIR_crea.repongoing_after_AKI.rda")


# for each creatinine count number of previous AKIs (any type)
crea.rep$NoAKIepisodes <- unlist(mclapply(X = 1:nrow(crea.rep),
                                          FUN = count_previous_AKIs,
                                          df = crea.rep,
                                          mc.cores = cores_number, 
                                          mc.silent = FALSE))


# for each creatinine count number of previous AKIs (stratified)
crea.rep$NoAKI1episodes <- unlist(mclapply(X = 1:nrow(crea.rep),
                                          FUN = count_previous_AKIs,
                                          df = crea.rep,
                                          aki_type = "AKI1",
                                          mc.cores = cores_number, 
                                          mc.silent = FALSE))


crea.rep$NoAKI2episodes <- unlist(mclapply(X = 1:nrow(crea.rep),
                                           FUN = count_previous_AKIs,
                                           df = crea.rep,
                                           aki_type = "AKI2",
                                           mc.cores = cores_number, 
                                           mc.silent = FALSE))


crea.rep$NoAKI3episodes <- unlist(mclapply(X = 1:nrow(crea.rep),
                                           FUN = count_previous_AKIs,
                                           df = crea.rep,
                                           aki_type = "AKI3",
                                           mc.cores = cores_number, 
                                           mc.silent = FALSE))


save(crea.rep, file = "SIR_crea.repongoing_after_AKI.rda")
