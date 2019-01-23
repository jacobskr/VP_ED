library(dplyr)
library(tidyverse)



sub <- claims %>%
  group_by(MRN_ALIAS, MEMBER_SEX) %>%
  summarize(AGE = max(MEMBER_AGE),
            CLAIM_YEARS = n_distinct(YEAR),
            NUM_VISITS = max(episode_seq),
            ALL_CLAIMS = n(),
            ED_NEEDED_0_ALL = sum(ED_NOT_NEEDED_PROP == 0),
            MEAN_EDNN_ALL = mean(ED_NOT_NEEDED_PROP),
            PREV_1_ALL = sum(PREVENTABILITY == 1),
            MEAN_PREV_ALL = mean(PREVENTABILITY),
            COST_ALL = sum(APPROVED_AMT))

ED_claims <- subset(claims, claims$SERVICE_TYPE %in% "ED")

ED_sub <- ED_claims %>%
  group_by(MRN_ALIAS, MEMBER_SEX) %>%
  summarize(ED_CLAIMS = n(),
            ED_NEEDED_0_ED = sum(ED_NOT_NEEDED_PROP == 0),
            MEAN_EDNN_ED = mean(ED_NOT_NEEDED_PROP),
            PREV_1_ED = sum(PREVENTABILITY == 1),
            MEAN_PREV_ED = mean(PREVENTABILITY),
            COST_ED = sum(APPROVED_AMT))

MEMBER_DATA <- full_join(sub, ED_sub, by = c("MRN_ALIAS", "MEMBER_SEX"), suffix = c(".all", ".ed"))
MEMBER_DATA <- MEMBER_DATA[,c(1,2,3,4,5,11,10,16,6,12,7,13,8,14,9,15)]


check <- filter(claims, claims$MRN_ALIAS == "23C9A67453F9")
