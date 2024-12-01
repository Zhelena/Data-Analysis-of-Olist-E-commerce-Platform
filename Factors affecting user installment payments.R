library(gplots)
payments <- read.csv('olist_order_payments_dataset.csv')
payments <- payments[,c(-1,-2)]
payments <- payments[payments$payment_type != "not_defined", ]

# Single variable, bad
ModelPrice <- lm(payment_installments ~ payment_value, data = payments)
summary(ModelPrice)
ModelType <- lm(payment_installments ~ payment_type, data = payments)
summary(ModelType)

# two variables, better
ModelPriceType <- lm(payment_installments ~ payment_value + payment_type, data = payments)
summary(ModelPriceType)

ModelInteract <- lm(payment_installments ~ payment_value + payment_type + payment_value * payment_type, data = payments)
summary(ModelInteract)

anova(ModelPriceType, ModelInteract)
# Choose interact


# 为什么不用plotmeans画C.I.：样本量太大，C.I.被近似成0

# payment_type受payment_value的影响吗
table(payments$payment_type)
mean1 <- aggregate(payment_value ~ payment_type, data = payments, FUN = mean)
mean1
aggregate(payments$payment_value, by=list(payments$payment_type),FUN=sd)
fit1 <- aov(payments$payment_value ~ payments$payment_type)
summary(fit1)

plot(mean1$payment_value, type = "b", pch = 1, xaxt = "n", 
     xlab = "Payment Type", ylab = "Payment Value",
     main = "Mean Plot")
axis(1, at = 1:length(mean1$payment_type), labels = mean1$payment_type)



# payment_installments受payment_value的影响吗
table(payments$payment_installments)
mean2 <- aggregate(payment_value ~ payment_installments, data = payments, FUN = mean)
mean2
aggregate(payments$payment_value, by=list(payments$payment_installments),FUN=sd)
fit2 <- aov(payments$payment_value ~ payments$payment_installments)
summary(fit2)

plot(mean2$payment_value, type = "b", pch = 1, xaxt = "n", 
     xlab = "Payment Type", ylab = "Payment Value",
     main = "Mean Plot")
axis(1, at = 1:length(mean2$payment_installments), labels = mean2$payment_installments)

