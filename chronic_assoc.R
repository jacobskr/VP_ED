library(arules)
library(tidyverse)
library(reshape2)

claims_sub <- read_rds("Data/CHRON_MBR.rds")



claims_fct <- claims_sub %>% ungroup() %>% select(-MRN_ALIAS, -MAX_AGE) 


claims_trns <- as(claims_fct, "transactions")



assoc_rules <- apriori(claims_fct,
                       parameter = list(supp = .0001, 
                                        conf = .8,
                                        maxtime = 100,
                                        maxlen = 7), 
                       appearance = list(rhs=c("TARGET=1")),
                       control = list(sort = -2))

# NEVER EVER RUN THIS WITHOUT HEAD UNLESS YOU HAVE NOT THAT MANY RULES
inspect(tail(sort(assoc_rules, by = "confidence")))


#How to get rules into a messy dataframe:
# test <- as(head(assoc_rules), "data.frame")
# 
# 
#This gets the RHS, but how to get LHS?
# test_lab <- labels(head(assoc_rules))
# test_l <- length(head(assoc_rules))
# 
# test_cut = unlist(strsplit(test_lab,"=> "))[seq(2,2*test_l,by=2)]
# print(test_cut)
# 
# #this gets all the LHS rules, but separates them.....
# assoc_rules@lhs@itemInfo$labels[assoc_rules@lhs@data@i+1]


# Get rid of redundant/subset rules
subsetRules <- which(colSums(is.subset(assoc_rules, assoc_rules)) > 1)

shorter_rules <- assoc_rules[-subsetRules]
rulesdf <- inspect(shorter_rules)

# Number of members these rules apply to
sum(rulesdf$count)

# Test significance of rules
sig <- is.significant(shorter_rules, claims_trns, alpha = .05)
length(sig[sig==TRUE])
inspect(shorter_rules[sig])

shorterdf <- as(shorter_rules[sig], "data.frame")
write.csv(shorterdf, "Data/assoc_data.csv")




casedf <- claims_sub %>% mutate(AGE_BIN = as.character(AGE_BIN),
                                SCORE = case_when(AGE_BIN==30&CHRONIC_SYS_2==1&CHRONIC_SYS_10==1  ~ 0.835616438356164,
                                                  AGE_BIN==30&CHRONIC_SYS_9==0&CHRONIC_SYS_17==1&CHRONIC_SYS_18==1  ~ 0.822222222222222,
                                                  AGE_BIN==30&CHRONIC_SYS_16==0&CHRONIC_SYS_17==1&CHRONIC_SYS_18==1  ~ 0.8,
                                                  MEMBER_SEX=="FEMALE"&AGE_BIN==30&CHRONIC_SYS_1==1&CHRONIC_SYS_5==1  ~ 0.8,
                                                  MEMBER_SEX=="FEMALE"&AGE_BIN==40&CHRONIC_SYS_3==1&CHRONIC_SYS_14==1  ~ 0.816326530612245,
                                                  AGE_BIN==40&CHRONIC_SYS_5==1&CHRONIC_SYS_10==1&CHRONIC_SYS_15==1  ~ 0.82,
                                                  AGE_BIN==30&CHRONIC_SYS_3==0&CHRONIC_SYS_8==0&CHRONIC_SYS_17==1&CHRONIC_SYS_18==1  ~ 0.8125,
                                                  PCP_V70_YEARLY_NONED=="N"&AGE_BIN==30&CHRONIC_SYS_8==0&CHRONIC_SYS_17==1&CHRONIC_SYS_18==1  ~ 0.804347826086957,
                                                  AGE_BIN==40&CHRONIC_SYS_3==1&CHRONIC_SYS_8==0&CHRONIC_SYS_13==0&CHRONIC_SYS_14==1  ~ 0.803571428571429,
                                                  AGE_BIN==30&CHRONIC_SYS_8==0&CHRONIC_SYS_11==0&CHRONIC_SYS_17==1&CHRONIC_SYS_18==1  ~ 0.804347826086957,
                                                  AGE_BIN==30&CHRONIC_SYS_3==1&CHRONIC_SYS_5==0&CHRONIC_SYS_6==1&CHRONIC_SYS_17==0  ~ 0.8,
                                                  AGE_BIN==30&CHRONIC_SYS_3==1&CHRONIC_SYS_5==0&CHRONIC_SYS_6==1&CHRONIC_SYS_13==0  ~ 0.8,
                                                  AGE_BIN==30&CHRONIC_SYS_3==1&CHRONIC_SYS_4==0&CHRONIC_SYS_5==0&CHRONIC_SYS_6==1  ~ 0.8,
                                                  AGE_BIN==30&CHRONIC_SYS_1==1&CHRONIC_SYS_2==0&CHRONIC_SYS_5==1&CHRONIC_SYS_10==0  ~ 0.803921568627451,
                                                  AGE_BIN==30&CHRONIC_SYS_2==0&CHRONIC_SYS_10==0&CHRONIC_SYS_11==1&CHRONIC_SYS_17==1  ~ 0.8,
                                                  AGE_BIN==30&CHRONIC_SYS_10==0&CHRONIC_SYS_11==1&CHRONIC_SYS_12==0&CHRONIC_SYS_17==1  ~ 0.808510638297872,
                                                  AGE_BIN==30&CHRONIC_SYS_11==1&CHRONIC_SYS_12==0&CHRONIC_SYS_17==1&CHRONIC_SYS_18==0  ~ 0.8,
                                                  AGE_BIN==30&CHRONIC_SYS_3==1&CHRONIC_SYS_5==0&CHRONIC_SYS_6==1&CHRONIC_SYS_18==0  ~ 0.8,
                                                  AGE_BIN==30&CHRONIC_SYS_1==1&CHRONIC_SYS_2==0&CHRONIC_SYS_5==1&CHRONIC_SYS_11==0&CHRONIC_SYS_17==0  ~ 0.804347826086957,
                                                  AGE_BIN==30&CHRONIC_SYS_2==0&CHRONIC_SYS_5==1&CHRONIC_SYS_12==0&CHRONIC_SYS_13==0&CHRONIC_SYS_17==1  ~ 0.8,
                                                  AGE_BIN==30&CHRONIC_SYS_1==1&CHRONIC_SYS_2==0&CHRONIC_SYS_5==1&CHRONIC_SYS_11==0&CHRONIC_SYS_13==0  ~ 0.804347826086957,
                                                  AGE_BIN==30&CHRONIC_SYS_4==0&CHRONIC_SYS_10==0&CHRONIC_SYS_11==1&CHRONIC_SYS_17==1&CHRONIC_SYS_18==0  ~ 0.804347826086957,
                                                  AGE_BIN==30&CHRONIC_SYS_1==1&CHRONIC_SYS_2==0&CHRONIC_SYS_5==1&CHRONIC_SYS_12==0&CHRONIC_SYS_14==0  ~ 0.804347826086957,
                                                  AGE_BIN==30&CHRONIC_SYS_1==1&CHRONIC_SYS_2==0&CHRONIC_SYS_5==1&CHRONIC_SYS_11==0&CHRONIC_SYS_14==0  ~ 0.818181818181818,
                                                  TRUE ~ 0))




claims_sm <- casedf %>% select(MRN_ALIAS, CHRONIC_SYS_3, AGE_BIN, CHRONIC_SYS_8, CHRONIC_SYS_17, CHRONIC_SYS_18, SCORE)
casedftest <- claims_sm %>% filter(AGE_BIN==30&CHRONIC_SYS_3==0&CHRONIC_SYS_8==0&CHRONIC_SYS_17==1&CHRONIC_SYS_18==1)


claims_all <- claims %>% select(MRN_ALIAS, MEMBER_SEX, MEMBER_AGE, starts_with("BODY_SYS_"))
