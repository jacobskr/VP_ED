library(dplyr)
library(tidyverse)
library(caret)

claims <- read_rds("Data/claimsCleanFull.RDS")


sub <- claims %>%
  mutate(TARGET = ifelse(SERVICE_TYPE == "ED", 1, 0)) %>%
  group_by(MRN_ALIAS, MEMBER_SEX) %>%
  summarize(AGE = max(MEMBER_AGE),
            TARGET = as.factor(max(TARGET)),
            CLAIM_YEARS = n_distinct(YEAR),
            NUM_VISITS = max(EPISODE_SEQ),
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
MEMBER_DATA[is.na(MEMBER_DATA)] <- 0

index_small <- createDataPartition(MEMBER_DATA$TARGET, p = 0.5, list = FALSE)
MEMBER_SMALL <- MEMBER_DATA[index_small, ]
index <- createDataPartition(MEMBER_SMALL$TARGET, p = 0.7, list = FALSE)

train_data <- MEMBER_SMALL[index, ]
train_sub <- train_data[,-1]
test_data  <- MEMBER_SMALL[-index, ]



fit_tree <- train(TARGET~ MEMBER_SEX + AGE + ALL_CLAIMS + COST_ALL,
        data= train_sub,
        method = "rpart")

# fit_log <- train(TARGET~ MEMBER_SEX + AGE + ALL_CLAIMS + ED_CLAIMS,
#                  data= train_sub,
#                  method = "glmnet")

tree_predict <- predict(object = fit_tree,
                       newdata = test_data)

confusionMatrix(data = tree_predict, reference = test_data$TARGET)

fit_tree_imp <- varImp(fit_tree, scale = FALSE)

tree_df <- as.data.frame(tree_predict)

pred_data <- bind_cols(test_data, tree_df )

fit_tree2 <- train(TARGET~ MEMBER_SEX + AGE + ALL_CLAIMS + COST_ALL,
                   data= train_sub,
                   method = "rpart")

