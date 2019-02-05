# 
# CODE = Diagnosis code as is with decimal
# CODE_FULL = Diagnosis code with decimal removed
# ED_NOT_NEEDED_PROP = Percent of cases where ED is not needed
# PREVENTABILITY = Percent of cases ED would have been prevented with prior intervention
# UNCLASSIFIED_ED = Percent of cases where ED score cannot be classified. IDK this one is weird.*
#   NumberOfClaims = Total count of claims CODE appears in
# NumberOfED = Total count of claims CODE appear in where SERVICE_TYPE == 'ED'
# NumberOfOTPT = Total count of claims CODE appear in where SERVICE_TYPE == 'OTPT'
# NumberOfINPT = Total count of claims CODE appear in where SERVICE_TYPE == 'INPT'
# AverageAGE = Average age of members who get diagnosed with this CODE
# Pct_Female = Percent female who get diagnosed with this CODE
# CODE_DESCRIPTION = ICD 9/10 description
# CHRONIC_INDICATOR = Yes/No/NA is Diagnosis considered chronic
# BODY_SYSTEM = 1-18 Categorization of CODE
# BODY_SYSTEM_DESC = Description of the 1-18 codes
# NumberOfED_NN = NumberOfED * ED_NOT_NEEDED_Prop**
#   NumberOfED_PRV = NumberOfED * PREVENTABILITY**
#   *You can interpret this if UNCLASSIFED_ED == 1 then VP has not created a scores for CODE
# ** If value = -1 then score was NA for CODE.  All CODE_2 - CODE-18*** that never appeared as a CODE_1 will not have a score and thus will have -1s for these columns. Some CODE_1s have NA scores as well but very few < 900
# ***To create this dataset I first gathered all 18 CODEs longways then aggreated until each line is a unique code.

library(dplyr)
library(tidyverse)
library(reshape2)

# Import and edit claims data -  Ryan's code
claimsFull <- read_rds("Data/claimsCleanFull.RDS")

all_diag_df <- claimsFull %>% 
  select(starts_with('CODE_'))

all_diag <- c(t(all_diag_df))

all_diag <- data.frame(table(all_diag))
rm(all_diag_df)

# Create vector of ICD-10 'V' codes
icdv <- str_subset(all_diag$all_diag, "[V]\\d{2}\\.") #Grab all codes that start with V
icd10v <-str_subset(icdv, "[X]") #ICD_10 "V" codes include "X"s

icd9 <- str_subset(all_diag$all_diag, "^\\d") #ICD-9 codes start with a number or a E or a V
icd9e <- str_subset(all_diag$all_diag, "[E]\\d{3}") #ICD-9 "E" codes have 3 digits before decimal

icd9Only <- all_diag %>%
  filter((all_diag %in% icd9 | all_diag %in% icd9e | all_diag %in% icdv) & !(all_diag %in% icd10v))

icd9vector <- icd9Only$all_diag 

# Add new un-punctuated codes and ICD version to claims data
claimsFull <- claimsFull %>%
  mutate(DIAG_CODE_1 = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
         ICD_V_1 = ifelse(CODE_1 %in% icd9vector, 9,10),
         DIAG_CODE_2 = str_remove_all(CODE_2, pattern = "[[:punct:]]"),
         ICD_V_2 = ifelse(CODE_2 %in% icd9vector, 9,10),
         DIAG_CODE_3 = str_remove_all(CODE_3, pattern = "[[:punct:]]"),
         ICD_V_3 = ifelse(CODE_3 %in% icd9vector, 9,10),
         DIAG_CODE_4 = str_remove_all(CODE_4, pattern = "[[:punct:]]"),
         ICD_V_4 = ifelse(CODE_4 %in% icd9vector, 9,10),
         DIAG_CODE_5 = str_remove_all(CODE_5, pattern = "[[:punct:]]"),
         ICD_V_5 = ifelse(CODE_5 %in% icd9vector, 9,10),
         DIAG_CODE_6 = str_remove_all(CODE_6, pattern = "[[:punct:]]"),
         ICD_V_6 = ifelse(CODE_6 %in% icd9vector, 9,10),
         DIAG_CODE_7 = str_remove_all(CODE_7, pattern = "[[:punct:]]"),
         ICD_V_7 = ifelse(CODE_7 %in% icd9vector, 9,10),
         DIAG_CODE_8 = str_remove_all(CODE_8, pattern = "[[:punct:]]"),
         ICD_V_8 = ifelse(CODE_8 %in% icd9vector, 9,10),
         DIAG_CODE_9 = str_remove_all(CODE_9, pattern = "[[:punct:]]"),
         ICD_V_9 = ifelse(CODE_9 %in% icd9vector, 9,10),
         DIAG_CODE_10 = str_remove_all(CODE_10, pattern = "[[:punct:]]"),
         ICD_V_10 = ifelse(CODE_10 %in% icd9vector, 9,10),
         DIAG_CODE_11 = str_remove_all(CODE_11, pattern = "[[:punct:]]"),
         ICD_V_11 = ifelse(CODE_11 %in% icd9vector, 9,10),
         DIAG_CODE_12 = str_remove_all(CODE_12, pattern = "[[:punct:]]"),
         ICD_V_12 = ifelse(CODE_12 %in% icd9vector, 9,10),
         DIAG_CODE_13 = str_remove_all(CODE_13, pattern = "[[:punct:]]"),
         ICD_V_13 = ifelse(CODE_13 %in% icd9vector, 9,10),
         DIAG_CODE_14 = str_remove_all(CODE_14, pattern = "[[:punct:]]"),
         ICD_V_14 = ifelse(CODE_14 %in% icd9vector, 9,10),
         DIAG_CODE_15 = str_remove_all(CODE_15, pattern = "[[:punct:]]"),
         ICD_V_15 = ifelse(CODE_15 %in% icd9vector, 9,10),
         DIAG_CODE_16 = str_remove_all(CODE_16, pattern = "[[:punct:]]"),
         ICD_V_16 = ifelse(CODE_16 %in% icd9vector, 9,10),
         DIAG_CODE_17 = str_remove_all(CODE_17, pattern = "[[:punct:]]"),
         ICD_V_17 = ifelse(CODE_17 %in% icd9vector, 9,10),
         DIAG_CODE_18 = str_remove_all(CODE_18, pattern = "[[:punct:]]"),
         ICD_V_18 = ifelse(CODE_18 %in% icd9vector, 9,10))

# Import and prepare diagnosis data
diag <- read_csv("Data/diagnosis_final.csv",
                 col_types = cols(.default = "c"))


diag["CHRONIC_INDICATOR"][is.na(diag["CHRONIC_INDICATOR"])] <- 0
diag["BODY_SYSTEM"][is.na(diag["BODY_SYSTEM"])] <- 0
diag["BODY_SYSTEM_DESC"][diag[,"BODY_SYSTEM"] == 0,] <- "UNSPECIFIED"
diag["CCS_CATEGORY"][is.na(diag["CCS_CATEGORY"])] <- 0
diag["CCS_CATEGORY_DESC"][diag[,"CCS_CATEGORY"] == 0,] <- "UNSPECIFIED"

bodysys <- diag[, c("DIAG_CODE", "ICD_VERSION", "BODY_SYSTEM", "CCS_CATEGORY", "CHRONIC_INDICATOR")]
bodysys$CCS_CATEGORY <- as.factor(as.character(bodysys$CCS_CATEGORY))
bodysys$BODY_SYSTEM <- as.factor(as.character(bodysys$BODY_SYSTEM))
bodysys$CHRONIC_INDICATOR <- as.factor(as.character(bodysys$CHRONIC_INDICATOR))



claims <- claimsFull %>%
  mutate(BODY_SYS_1 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_1 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_2 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_2 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_3 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_3 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_4 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_4 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_5 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_5 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_6 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_6 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_7 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_7 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_8 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_8 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_9 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_9 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_10 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_10 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_11 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_11 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_12 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_12 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_13 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_13 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_14 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_14 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_15 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_15 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_16 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_16 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_17 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_17 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_18 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_18 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA))


# claims_diag <- merge(claims,
#                      bodysys[,c("CODE","BODY_SYSTEM")],
#                      by.x = "CODE_1",
#                      by.y = "CODE",
#                      all.x = TRUE,
#                      sort = FALSE,
#                      no.dups = FALSE)
# 
# 
# codes <- NULL
# 
# for (i in seq(1:18)){
#   temp <- paste("CODE_",i, sep = "")
#   codes <- cbind(test, temp)
# }



# Combine claims and diagnosis data
# claims_diag <- matrix()
# 
# for (i in seq(1:18)){
#   temp <- merge(claimsFull[,c(paste("DIAG_CODE_",i,sep=""),
#                               paste("ICD_V_",i,sep=""))],
#                 bodysys[,c("DIAG_CODE",
#                            "ICD_VERSION",
#                            "BODY_SYSTEM")],
#                 by.x = c(paste("DIAG_CODE_",i,sep=""),
#                          paste("ICD_V_",i,sep="")),
#                 by.y = c("DIAG_CODE",
#                          "ICD_VERSION"),
#                 all.x = TRUE,
#                 sort = FALSE,
#                 no.dups = FALSE)
#   
#   claims_diag <- cbind(claims_diag, temp)
# }






