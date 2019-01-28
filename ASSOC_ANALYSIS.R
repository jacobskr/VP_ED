library(dplyr)
library(tidyverse)
library(caret)
library(arules)

claims <- read_rds("Data/claimsCleanFull.RDS")
claims["PCP_ID"][is.na(claims["PCP_ID"])] <- 0


claims_sub <- claims %>%
  mutate(TARGET = ifelse(SERVICE_TYPE == "ED" & ED_NOT_NEEDED_PROP > 0.9, 1, 0)) %>%
  mutate(AGE_BIN = as.factor(cut(MEMBER_AGE, breaks = seq(0,90, by =10)))) %>% 
  group_by(MRN_ALIAS, MEMBER_SEX, AGE_BIN) %>%
  summarize(AGE = max(MEMBER_AGE),
            TARGET = as.factor(max(TARGET)),
            CLAIM_YEARS = n_distinct(YEAR),
            NUM_VISITS = max(EPISODE_SEQ),
            ALL_CLAIMS = n(),
            PCP_VISITS = sum(PCP_ID == VENDOR_PROV_ID),
            V70.0_VISITS = sum(CODE_1 == "V70.0"),
            PCP_V70_VISITS_NONED = sum(PCP_ID == VENDOR_PROV_ID | (CODE_1 == "V70.0" & SERVICE_TYPE != "ED")),
            PCP_V70_YEARLY_NONED = as.factor(ifelse((sum(PCP_ID == VENDOR_PROV_ID | (CODE_1 == "V70.0" & SERVICE_TYPE != "ED"))/n_distinct(YEAR))>=1,"Y","N")),
            V70.0_ED_VISITS = sum(CODE_1 == "V70.0" & SERVICE_TYPE == "ED"),
            V70.0_ED_VISITS_BOOL = as.factor(ifelse(sum(CODE_1 == "V70.0" & SERVICE_TYPE == "ED") > 0, "Y", "N")),
            ED_CLAIMS = sum(SERVICE_TYPE == "ED"),
            EDNN_1_ALL = sum(ED_NOT_NEEDED_PROP == 1),
            MEAN_EDNN_ALL = mean(ED_NOT_NEEDED_PROP),
            PREV_1_ALL = sum(PREVENTABILITY == 1),
            MEAN_PREV_ALL = mean(PREVENTABILITY),
            COST_ALL = sum(APPROVED_AMT))

# ED_claims <- subset(claims, claims$SERVICE_TYPE %in% "ED")
# 
# ED_sub <- ED_claims %>%
#   group_by(MRN_ALIAS, MEMBER_SEX) %>%
#   summarize(ED_CLAIMS = n(),
#             EDNN_1_ED = sum(ED_NOT_NEEDED_PROP == 1),
#             MEAN_EDNN_ED = mean(ED_NOT_NEEDED_PROP),
#             PREV_1_ED = sum(PREVENTABILITY == 1),
#             MEAN_PREV_ED = mean(PREVENTABILITY),
#             COST_ED = sum(APPROVED_AMT))
# 
# MEMBER_DATA <- full_join(sub, ED_sub, by = c("MRN_ALIAS", "MEMBER_SEX"), suffix = c(".all", ".ed"))

claims_fct <- claims_sub[,c(2, 3, 12, 14, 5)]


claims_trns <- as(claims_fct, "transactions")

assoc_rules <- apriori(claims_fct,
                       parameter = list(supp = .001, 
                                        conf = .2), 
                       appearance = list(rhs=c("TARGET=1")))

inspect(assoc_rules, by = "confidence")
