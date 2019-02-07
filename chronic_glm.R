library(dplyr)
library(caret)
library(caTools)
library(aod)

claims_sub <- read_rds("Data/CHRON_MBR.rds")

#Split data into train and test sets
set.seed(123)
sample = sample.split(claims_sub$MRN_ALIAS, SplitRatio = .75)
train = subset(claims_sub, sample == TRUE)
test = subset(claims_sub, sample == FALSE)

#Ensure you captured all data
# nrow(claims_sub) == nrow(train) + nrow(test)

#Logistic Regression
  # CHRONIC_SYS_0 and NONE only have 1 factor, take them out
chronic_logit <- glm(formula = TARGET ~ MEMBER_SEX + AGE_BIN + PCP_V70_YEARLY_NONED + CHRONIC_SYS_1 + CHRONIC_SYS_2 + CHRONIC_SYS_3 +
                       CHRONIC_SYS_4 + CHRONIC_SYS_5 + CHRONIC_SYS_6 + CHRONIC_SYS_7 + CHRONIC_SYS_8 + CHRONIC_SYS_9 + CHRONIC_SYS_10 +
                       CHRONIC_SYS_11 + CHRONIC_SYS_12 + CHRONIC_SYS_13 + CHRONIC_SYS_14 + CHRONIC_SYS_15 + CHRONIC_SYS_16 +
                       CHRONIC_SYS_17 + CHRONIC_SYS_18,
                     family = "binomial",
                     data = train)

summary(chronic_logit)
wald.test(b = coef(chronic_logit), Sigma = vcov(chronic_logit), Terms = 4:6)

glm.fit()