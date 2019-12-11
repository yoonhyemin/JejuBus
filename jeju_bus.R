####################################################################################################################################
#jeju-bus
#set-up
####################################################################################################################################
setwd("D:/2019/etc/DATA CONTEST/jeju_bus")
getwd()
#install.packages("reshape")
library(reshape)
library(dplyr)
####################################################################################################################################
#train-call
#preprocessing
####################################################################################################################################
raw_data <-read.csv("train.csv")
str(raw_data)
#melt-cast
pre_data <- raw_data[,-c(15:20)]
station_loc <- raw_data[,c(7:8)]
str(pre_data)
str(station_loc)
m_train <- melt(data = pre_data, 
                id.vars = c("date", "bus_route_id","latitude","longitude"), 
                measure.vars = c("X6.7_ride","X7.8_ride","X8.9_ride","X9.10_ride","X10.11_ride","X11.12_ride"))
#                measure.vars = c("X6.7_ride","X7.8_ride","X8.9_ride","X9.10_ride","X10.11_ride","X11.12_ride","X6.7_takeoff","X7.8_takeoff","X8.9_takeoff","X9.10_takeoff","X10.11_takeoff","X11.12_takeoff","X18.20_ride"))
str(m_train)
write.csv(station_loc,file = "station_loc.csv")
dayweek <-read.csv("dayofweek.csv")
str(dayweek)
j_train <- left_join(m_train,dayweek,by = c("date","date"))
str(j_train)
head(j_train)
write.csv(j_train,file = "j_train.csv")
#test data set
raw_test <-read.csv("test.csv")
str(raw_test)
#melt-cast
m_test <- melt(data = raw_test, 
                id.vars = c("date", "bus_route_id","latitude","longitude"), 
                measure.vars = c("X6.7_ride","X7.8_ride","X8.9_ride","X9.10_ride","X10.11_ride","X11.12_ride","X6.7_takeoff","X7.8_takeoff","X8.9_takeoff","X9.10_takeoff","X10.11_takeoff","X11.12_takeoff","X18.20_ride"))
str(m_test)
#write.csv(m_test,file = "m_test.csv")
dayweek <-read.csv("dayofweek.csv")
str(dayweek)
j_test <- left_join(m_test,dayweek,by = c("date","date"))
str(j_test)
head(j_test)

####################################################################################################################################
# step_02-2
#RandomForest 인자찾기(y- linear)
library(caret)
library(randomForest)
#현재 사용하고 있는 메모리를 확인
memory.size()
#R에서 최대 가상 메모리
memory.limit()
#R에서 최대 가상 메모리를 5000mb로
memory.limit(5000)
memory.limit()
####################################################################################################################################
#write.csv(train,file = "train_exp_1.csv" )
train <- j_train
str(train)
train <- train[,-c(1)]
#train <- train[,-c(1)]
#hist(train$SalePrice)

set.seed(777)
train_idx <- sample(seq_len(nrow(train)),floor(nrow(train)*0.02))
#train_idx
sample_data <- train[train_idx,]
str(sample_data)
sam_train_idx <- sample(seq_len(nrow(sample_data)),floor(nrow(sample_data)*0.5))
p_train <- train[sam_train_idx,]
p_test <- train[-sam_train_idx,]

str(p_train)
train_rf <- randomForest(value~.,data=p_train)
train_rf
train_rf$importance
varImpPlot(train_rf)

#검증
#정확도(train)
rf_pr_train <- predict(train_rf,newdata=p_train)
#rf_pr_train
actual <- p_train$value
preds <- rf_pr_train
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq
plot(preds,actual)

#정확도(test)
rf_pr_test <- predict(train_rf,newdata=p_test)

actual_t <- p_test$value
preds_t <- rf_pr_test
rss <- sum((preds_t - actual_t) ^ 2)
tss <- sum((actual_t - mean(actual_t)) ^ 2)
rsq <- 1 - rss/tss
rsq
plot(preds_t,actual_t)


###################################################################################################################################
# step_03
#regression
###################################################################################################################################

#importance 상위인자로 수행
train_rf_norminal$importance
varImpPlot(train_rf_norminal)
import_val <- names(train_rf$importance[train_rf$importance > 16.4,])
import_val

val_train <- p_train[,c(import_val,'SalePrice')]
str(val_train)
val_test <- p_test[,c(import_val,'SalePrice')]
str(val_test)

p_train2 <- val_train
p_test2 <- val_test


#전체 변수로 수행
p_train2 <- p_train
p_test2 <- p_test
str(p_train2)

#데이터 셋 구성
x_train <- model.matrix(value ~., p_train2)[,-1]
x_train <- as.data.frame(x_train)
y_train <- p_train2$value
x_test<-model.matrix(value~., p_test2)[,-1]
x_test <- as.data.frame(x_test)
y_test<- p_test2$value
class(p_train)

lm_passin<-lm(formula = y_train ~ . , data = x_train)
summary(lm_passin)




#########################################################################################
#lasso
library(glmnet)
#########################################################################################

#데이터 셋 구성
x_train <- model.matrix(value ~., p_train2)[,-1]
y_train <- p_train2$value
x_test<-model.matrix(value~., p_test2)[,-1]
y_test<- p_test2$value

lambda_seq <- 10^seq(2, -2, by = -.01)
#lambda_seq
cv.lasso <- cv.glmnet(x_train, y_train, alpha=1, lambda = lambda_seq)
plot(cv.lasso)
cv.lasso
best_lam <- cv.lasso$lambda.1se
best_lam <- cv.lasso$lambda.min
best_lam

lasso_best <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam)
lasso_best
coef(lasso_best)

pred_tr <- predict(lasso_best, s = best_lam, newx = x_train)
#pred_tr
final_tr <- cbind(y_train, pred_tr)
plot(final_tr)
actual <- y_train
preds <- pred_tr
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

#p-value
lasso_pv <- as.matrix(coef(lasso_best))
lasso_pv
keep_X <- rownames(lasso_pv)[lasso_pv!=0]
keep_X <- keep_X[!keep_X == "(Intercept)"]
X <- x_train[,keep_X]
keep_X
lm_pv <- lm(log(y_train)~X)
summary(lm_pv)

par(mfrow = c(2, 2))
plot(lm_pv)


final_tr <- as.data.frame(final_tr)
str(final_tr)
coef(lasso_best)
seq<-c(1:936)
plot_lasso <- cbind(seq,final_tr)
names(plot_lasso) <- c("seq","origin_value","predict_value")

#ggplot(data = train, aes(x=seq , y=good.bad, group = 1)) +
#  geom_line(linetype="solid", size=1, colour="red") +
#  geom_point(size=2, shape=20, colour="red") 

g <- ggplot(data = plot_lasso, aes(x = seq))
g <- g + geom_line(aes(y = plot_lasso$origin_value),linetype="solid", colour = "blue", size = 1)
g <- g + geom_line(aes(y = plot_lasso$predict_value),linetype="solid", colour = "red", size = 0.5)
#g <- g + geom_line(aes(y = train$SD_TAC1_Accum),linetype=2, colour = "green", size = 0.5)
#g <- g + geom_abline(intercept= 1812.9, slope=-0.1904, color='black', size = 0.5)
g


#lasso 검증
pred_te <- predict(lasso_best, s = best_lam, newx = x_test)

final_te <- cbind(y_test, pred_te)
#str(final_te)
# Checking the first six obs
plot(final_te)

actual <- y_test
preds <- pred_te
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

#차트그리기
seq<-c(1:292)
test <- cbind(seq,final_te)
head(test)
names(test) <- c("seq","origin_value","predict_value")
test <- as.data.frame(test)
str(test)
#ggplot(data = train, aes(x=seq , y=good.bad, group = 1)) +
#  geom_line(linetype="solid", size=1, colour="red") +
#  geom_point(size=2, shape=20, colour="red") 

g <- ggplot(data = test, aes(x = seq))
g <- g + geom_line(aes(y = test$origin_value),linetype="solid", colour = "blue", size = 1)
g <- g + geom_line(aes(y = test$predict_value),linetype="solid", colour = "red", size = .5)
#g <- g + geom_line(aes(y = train$SD_TAC1_Accum),linetype=2, colour = "green", size = 0.5)
#g <- g + geom_abline(intercept= 1812.9, slope=-0.1904, color='black', size = 0.5)
g


#########################################################################################
#test 검증

str(test)

#NA 치환(mean)                                                                                                                              

#정확도(random forest - test)
rf_val_test <- predict(train_rf,newdata=test)
rf_val_test <- as.data.frame(rf_val_test)
str(rf_val_test)
names(rf_val_test) <- c("SalePrice")
write.csv(rf_val_test,file = "test_submission_rf.csv" )

actual <- test$SalePrice
preds <- rf_val_test
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq
plot(preds,actual)


#lasso
x_test_fr<-model.matrix(SalePrice~., test)[,-1]
y_test_fr<- test$SalePrice

pred_te <- predict(lasso_best, s = best_lam, newx = x_test_fr)
names(pred_te) <- c("Id","SalePrice")
pred_te <- as.data.frame(pred_te)
final_te <- cbind(y_test_fr, pred_te)
plot(final_te)

write.csv(pred_te,file = "test_submission_ls.csv" )

actual <- y_test_fr
preds <- pred_te
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq
