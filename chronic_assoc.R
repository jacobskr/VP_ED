library(arules)
library(dplyr)
library(tidyverse)
library(reshape2)

claims_sub <- read_rds("Data/CHRON_MBR.rds")



claims_fct <- claims_sub[,c(2:ncol(claims_sub))]


claims_trns <- as(claims_fct, "transactions")



assoc_rules <- apriori(claims_fct,
                       parameter = list(supp = .00001, 
                                        conf = .8,
                                        maxtime = 100,
                                        maxlen = 7), 
                       appearance = list(rhs=c("TARGET=1")),
                       control = list(sort = -2))

# NEVER EVER RUN THIS WITHOUT HEAD UNLESS YOU HAVE NOT THAT MANY RULES
inspect(head(sort(assoc_rules, by = "confidence")))


# #How to get rules into a messy dataframe:
# test <- as(head(assoc_rules), "data.frame")
# 
# 
# #This gets the RHS, but how to get LHS?
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

