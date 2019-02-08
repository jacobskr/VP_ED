library(randomForest)
library(caTools)

#Import Data and get rid of records with NAs (need to find source of NAs at some point)
claims_sub <- read_rds("Data/CHRON_MBR.rds")
claims_sub <- claims_sub[, - c(6,25)]
claims_sub <- subset(claims_sub, is.na(claims_sub$AGE_BIN) == FALSE)
claims_sub <- subset(claims_sub, is.na(claims_sub$PCP_V70_YEARLY_NONED) == FALSE)

#Train Test
set.seed(123)
sample = sample.split(claims_sub$MRN_ALIAS, SplitRatio = .75)
train = subset(claims_sub, sample == TRUE)
test = subset(claims_sub, sample == FALSE)

# Create Model... Need to run this later due to space/RAM issues -  also add other stats diagnostics 
rfmodel <- randomForest(TARGET ~ . - MRN_ALIAS, data = train)

rfpred <- predict(rfmodel, newdata = test)
table(rfpred, test$TARGET)
