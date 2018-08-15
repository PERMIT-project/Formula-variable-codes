crea.rep <- crea.rep %>% 
  mutate(CKDStage3to5_MDRDeGFR = ifelse(CKDStage_MDRDeGFR %in% c("3a", "3b", "4", "5"),
                               TRUE,
                               FALSE),
         CKDStage_instant = NA_character_,
         CKDStage_instant = ifelse(MDRDeGFR >= 90,
                                   "1",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(MDRDeGFR >= 60 & MDRDeGFR < 90,
                                   "2",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(MDRDeGFR >= 45 & MDRDeGFR < 60,
                                   "3a",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(MDRDeGFR >= 30 & MDRDeGFR < 45,
                                   "3b",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(MDRDeGFR >= 15 & MDRDeGFR < 30,
                                   "4",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(MDRDeGFR < 15,
                                   "5",
                                   CKDStage_instant))

colnames(crea.rep)[colnames(crea.rep) == "CKDStage_instant"] <- "CKDStage_instant_MDRDeGFR"
################################################

crea.rep <- crea.rep %>% 
  mutate(CKDStage3to5_CKDEPIeGFR = ifelse(CKDStage_CKDEPIeGFR %in% c("3a", "3b", "4", "5"),
                                        TRUE,
                                        FALSE),
         CKDStage_instant = NA_character_,
         CKDStage_instant = ifelse(CKDEPIeGFR >= 90,
                                   "1",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(CKDEPIeGFR >= 60 & CKDEPIeGFR < 90,
                                   "2",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(CKDEPIeGFR >= 45 & CKDEPIeGFR < 60,
                                   "3a",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(CKDEPIeGFR >= 30 & CKDEPIeGFR < 45,
                                   "3b",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(CKDEPIeGFR >= 15 & CKDEPIeGFR < 30,
                                   "4",
                                   CKDStage_instant),
         CKDStage_instant = ifelse(CKDEPIeGFR < 15,
                                   "5",
                                   CKDStage_instant))

colnames(crea.rep)[colnames(crea.rep) == "CKDStage_instant"] <- "CKDStage_instant_CKDEPIeGFR"

