library(e1071)
library(caTools)

claims_svm <- readRDS("Data/claims_body_as_code_with_lead_service.rds")
claims_svm <- claims_svm %>% select(NEXT_SERVICE, MEMBER_SEX, AGE_GROUP,starts_with("bs_"), starts_with("CODE_"))
claims_svm[is.na(claims_svm)] <- 0

# https://www.r-bloggers.com/machine-learning-using-support-vector-machines/
set.seed(123)
sample = sample.split(claims_svm, SplitRatio = .75)
train = subset(claims_svm, sample == TRUE)
test = subset(claims_svm, sample == FALSE)

#Create support vector machine
svmmodel <- svm(NEXT_SERVICE ~ . , train)

# Run it on test data
svmpred <- predict(model_svm, test)

points(train$x, pred, col = "blue", pch=4)

#Wondering if this will work since it is boolean target and not numeric
svmerror <- svmmodel$residuals

error_2 <- train$y - pred

svm_error <- sqrt(mean(error_2^2))


# Can then tune the svm then from the tune get the $best.model


