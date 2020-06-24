

#formulas
formula = Success~.

#Best Model with duration included
formula1 = Success~ age  + month + duration + poutcome + emp.var.rate + cons.price.idx + education + factor(contact=='telephone') + factor(day_of_week=='wed') + campaign + pdays + euribor3m + cons.conf.idx 
formula2 = Success~ age  + month + poutcome + emp.var.rate + cons.price.idx  + factor(contact=='telephone') + factor(day_of_week=='wed') + campaign + pdays + euribor3m
formula3 = Success~ age  + factor(month=='aug') + poutcome + emp.var.rate + cons.price.idx + factor(contact=='telephone') + factor(day_of_week=='wed') + campaign + euribor3m + factor(education=='low') + pdays + cons.conf.idx
formula4 = Success~ age  + month + duration +  poutcome + emp.var.rate + cons.price.idx  + factor(contact=='telephone') + factor(day_of_week=='wed') + campaign + euribor3m + education + pdays + cons.conf.idx


#Regressions
data <- glm(formula, data=train, family = binomial)
data1 <- glm(formula1, data=train, family = binomial)
data2 <- glm(formula2, data=train, family = binomial)
data3 <- glm(formula3, data=train, family = binomial)
data4 <- glm(formula4, data=train, family = binomial)

#Summaries
summary(data)
summary(data1)
summary(data2)
summary(data3)
summary(data4)

bank$Success <- as.numeric(bank$Success)

#residuals

binnedplot(fitted(data4),residuals(data4, type = "response"),  nclass = NULL,  xlab = "Expected Values",  ylab = "Average residual",  main = "Binned residual plot for Model 5",  cex.pts = 0.7,  col.pts = 2,  col.int = "blue")


library(lmtest)
lrtest(data3,data4, data2)




#Model1
predict <- predict(data4, type = 'response')
model_1_table <- table(train$Success, predict > 0.5)

ROCRpred1 <- prediction(predict, train$Success)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7))
acc_pruned <- sum(diag(model_1_table)) / sum(model_1_table)
acc_pruned


#Model2

predict <- predict(data2, type = 'response')
model_2_table <- table(bank$Success, predict > 0.5)
acc_pruned <- sum(diag(model_2_table)) / sum(model_2_table)
acc_pruned

ROCRpred2 <- prediction(predict2, bank$Success)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7))



#Model3

predict <- predict(data3, type = 'response')
model_3_table <- table(bank$Success, predict > 0.5)

acc_pruned <- sum(diag(model_3_table)) / sum(model_3_table)
acc_pruned

ROCRpred3 <- prediction(predict3, bank$Success)
ROCRperf3 <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf3, colorize = TRUE, text.adj = c(-0.2,1.7))

#Model4

predict <- predict(data4, type = 'response')
model_4_table <- table(bank$Success, predict > 0.5)

acc_pruned <- sum(diag(model_4_table)) / sum(model_4_table)
acc_pruned

ROCRpred4 <- prediction(predict4, bank$Success)
ROCRperf4 <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf4, colorize = TRUE, text.adj = c(-0.2,1.7))
title('Model 4')

anova(data2,data3,data4, test = "Chisq")
varImp(data, sort=TRUE)
plot(gbmImp)


opar <- par()
par(mfrow = c(2,2))



#Oversampling
data_balanced_over <- ovun.sample(Success~ age  + month + poutcome + emp.var.rate + cons.price.idx  + contact + day_of_week + campaign + euribor3m + education + pdays + cons.conf.idx, data = train, method = "over",N = 32950)$data
table(data_balanced_over$Success)

#undersampling
data_balanced_under <- ovun.sample(Success~ age  + month + poutcome + emp.var.rate + cons.price.idx  + contact + day_of_week + campaign + euribor3m + education + pdays + cons.conf.idx, data = train, method = "under", N = 15000, seed = 1)$data
table(data_balanced_under$Success)

#both
data_balanced_both <- ovun.sample(Success~ age  + month + poutcome + emp.var.rate + cons.price.idx  + contact + day_of_week + campaign + euribor3m + education + pdays + cons.conf.idx, data = train, method = "both", p=0.5,                             N=1000, seed = 1)$data
table(data_balanced_both$Success)

#rose
data.rose <- ROSE(Success~ age  + month + poutcome + emp.var.rate + cons.price.idx  + contact + day_of_week + campaign + euribor3m + education + pdays + cons.conf.idx, data = train, seed = 1)$data
table(data.rose$Success)

#splitting the data set

set.seed(2018)

# create a 70/30 training/test set split
n_rows <- nrow(bank)
# sample 70% (n_rows * 0.7) indices in the ranges 1:nrows
training_idx <- sample(n_rows, n_rows * 0.8)
# filter the data frame with the training indices (and the complement)
train <- bank[training_idx,]
test <- bank[-training_idx,]

-------------------------------------------
x <- bank[,11:13]
y <- bank[,21]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
---------------------------------------------------------
  ctrl_parameters <- trainControl(method = 'CV', number = 10)

  titanic_glm<- train(formula2, data = train, method = "glm", trControl = ctrl_parameters, metric="Accuracy")

p1 <- predict(titanic_glm, data = train)
confusionMatrix(p1, data = test)

