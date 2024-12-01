library(gplots)
score <- read.csv('score.csv')

# 为什么不用plotmeans画C.I.：样本量太大，C.I.被近似成0
# 评分受订单金额影响吗
attach(score)
table(review_score)
mean3 <- aggregate(payment_value ~ review_score, FUN = mean)
mean3
aggregate(payment_value, by = list(review_score),FUN = sd)
fit3 <- aov(payment_value ~ review_score)
summary(fit3)

plot(mean3$payment_value, type = "b", pch = 1, xaxt = "n", 
     xlab = "Review Score", ylab = "Payment Value",
     main = " ")
axis(1, at = 1:length(mean3$review_score), labels = mean3$review_score)
detach(score)

# Linear Model--------significant but not linear relationship
cor(score$review_score, score$payment_value)
ModelValue <- lm(review_score ~ payment_value, data = score)
summary(ModelValue)



# 评分受收货时间影响吗
attach(score)
table(review_score)
mean4 <- aggregate(deliver_time, by = list(review_score), FUN = mean)
mean4
aggregate(deliver_time, by = list(review_score), FUN = sd)
fit4 <- aov(deliver_time ~ review_score)
summary(fit4)
mean4 <- aggregate(deliver_time ~ review_score, FUN = mean)

plot(mean4$deliver_time, type = "b", pch = 1, xaxt = "n", 
     xlab = "Review Score", ylab = "Delivery time",
     main = " ")
axis(1, at = 1:length(mean4$review_score), labels = mean4$review_score)
detach(score)

# Linear Model-------------importance of deliver!
cor(score$review_score, score$deliver_time)
ModelTime <- lm(review_score ~ deliver_time, data = score)
summary(ModelTime)




# 评分对复购的影响
score_reorder <- read.csv('score_reorder.csv')
Model <- lm(score_reorder$count ~ score_reorder$review_score)
summary(Model)




