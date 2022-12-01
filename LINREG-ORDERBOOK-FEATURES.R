# Data
d <- read.csv("regdf.csv")
# General model
model <- glm(
    upordown~imb+log_return_bid+log_volume_ask+log_return_mid_price+log_ask_div_bid+
    log_volume_bid+log_return_ask+log_volume_ask_div_bid, 
    family=binomial,data=d) 
summary(model)
# Removing log_return_mid_price due to non-significance
goodmodel <- glm(
    upordown~imb+log_return_bid+log_volume_ask+log_ask_div_bid+log_volume_bid + log_return_ask,
    family=binomial,data=d) 
summary(goodmodel)
# All features significant

d$predprob<-round(fitted(goodmodel),2)
plot(d$predprob)
classificationtable<-table(d$upordown,d$predprob > 0.5)
classificationtable
# 26'026 guess: down true: down - correct 
# 29'950 guess: up true: down  - false negative
# 342'516 guess: down true: up - false positive
# 792'030 guess: up true: up - correct

# Correct guesses for market up movement
sensitivity<-(classificationtable[2,2]/(classificationtable[2,2]+classificationtable[2,1]))*100
sensitivity # 96.36%
# Correct guesses for market down movement
specificity<-(classificationtable[1,1]/(classificationtable[1,1]+classificationtable[1,2]))*100
specificity # 7 %
# Accuracy
classificationtable
100 * (classificationtable[1] + classificationtable[4]) / nrow(d)
# 68.71%

# Result: Unacceptable amount of false positives
