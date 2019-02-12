library(e1071)
library(caTools)


# https://www.r-bloggers.com/machine-learning-using-support-vector-machines/
set.seed(123)
sample = sample.split(claims_sub$MRN_ALIAS, SplitRatio = .75)
train = subset(claims_sub, sample == TRUE)
test = subset(claims_sub, sample == FALSE)

#Create support vector machine
svmmodel <- svm(TARGET ~ . - MRN_ALIAS, train)

# Run it on test data
svmpred <- predict(model_svm, test)

points(train$x, pred, col = "blue", pch=4)

#Wondering if this will work since it is boolean target and not numeric
svmerror <- svmmodel$residuals

error_2 <- train$y - pred

svm_error <- sqrt(mean(error_2^2))


# Can then tune the svm then from the tune get the $best.model


