library(arules)

claims_sub_assoc <- readRDS("Data/claims_body_as_code_with_lead_service.rds")
claims_sub_assoc <- claims_sub %>% select(NEXT_SERVICE, MEMBER_SEX, AGE_GROUP,starts_with("bs_"), starts_with("CODE_"))
claims_sub_assoc <- claims_sub[!is.na(claims_sub$NEXT_SERVICE),]


# tid <- as.character(claims_sub[['MRN_ALIAS']])
# df <- claims_sub[,-1]


trans <- as(claims_sub_assoc, "transactions")

#transactionInfo(trans)[["transactionID"]] <- tid



assoc_rules <- apriori(trans,
                       parameter = list(supp = .000001, 
                                        conf = .5,
                                        maxtime = 300,
                                        maxlen = 8), 
                       appearance = list(rhs=c("NEXT_SERVICE=ED")),
                       control = list(sort = -2))

inspect(head(sort(assoc_rules, by = "count"),20))


subsetRules <- which(colSums(is.subset(assoc_rules, assoc_rules)) > 1)

shorter_rules <- assoc_rules[-subsetRules]
rulesdf <- inspect(shorter_rules)

# Number of members these rules apply to
sum(rulesdf$count)

# Test significance of rules
sig <- is.significant(shorter_rules, claims_trns, alpha = .05)
length(sig[sig==TRUE])
inspect(shorter_rules[sig])

