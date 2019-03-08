library(dplyr)
library(caret)
library(caTools)
library(aod)
library(pROC)
library(bigglm)

claims_sub <- readRDS("Data/claims_body_as_code_with_lead_service.rds")
claims_sub <- claims_sub %>% select(NEXT_SERVICE, MEMBER_SEX, AGE_GROUP,starts_with("bs_"), starts_with("CODE_"))
claims_sub[is.na(claims_sub)] <- 0
claims_sub <- claims_sub[!is.na(claims_sub$NEXT_SERVICE),]
cols <- names(claims_sub)
claims_sub$AGE_GROUP <- as.factor(claims_sub$AGE_GROUP)
claims_sub[,4:ncol(claims_sub)] <- lapply(claims_sub[,4:ncol(claims_sub)], as.character)
claims_sub[,4:ncol(claims_sub)] <- lapply(claims_sub[,4:ncol(claims_sub)], factor)


#Split data into train and test sets
set.seed(123)
sample = sample.split(claims_sub$NEXT_SERVICE, SplitRatio = .75)
train = subset(claims_sub, sample == TRUE)
test = subset(claims_sub, sample == FALSE)

#Ensure you captured all data
# nrow(claims_sub) == nrow(train) + nrow(test)

#Logistic Regression
  # CHRONIC_SYS_0 and NONE only have 1 factor, take them out
chronic_logit <- bigglm(formula = NEXT_SERVICE ~ .,
                     family = "binomial",
                     data = train)

summary(chronic_logit)
anova(chronic_logit, test = "Chisq")
wald.test(b = coef(chronic_logit), Sigma = vcov(chronic_logit), Terms = 4:6)

#Fit model to test data
fitted.results <- predict(object = chronic_logit,
                          newdata = test[,-c(1,6,25)],
                          type = 'response')
fitted.results <- ifelse(fitted.results > .5, 1, 0)

#Misclassifier Error
mce <- length(test$TARGET[fitted.results != test$TARGET])/length(test$TARGET)
print(paste('Accuracy:',1-mce))

#ROC Curve
groc <- roc(fitted.results, as.numeric(test$TARGET))
plot(groc)


