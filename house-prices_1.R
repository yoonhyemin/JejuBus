#####################################################################################################################################
# step_00
# House-prices
# MODEL : Regression

# 폴더위치
setwd("D:/2019/기타/데이터분석 콘테스트/인연")
getwd()

#install.packages
#install.packages("rpart.plot")
#install.packages("ggplot2")
#install.packages("randomForest")
#install.packages("glmnet")
#install.packages("psych")
#install.packages("car")
#call library
library(psych)
library(car)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(MASS)
library(glmnet)

#sessionInfo() 

#####################################################################################################################################
# step_01
# Data Preprocessing
#####################################################################################################################################
#데이터 호출
train <-read.csv("train.csv")
test <-read.csv("test.csv")
all_data = rbind(train,test)
str(all_data)
SalePrice <- all_data$SalePrice
#all_data <- all_data[,-c(1)]
all_data <- all_data[,-c(1,7,58,73:75,81)]
# 데이터 타입 변경

#str(SalePrice)
all_data$MSSubClass <- as.factor(all_data$MSSubClass)
all_data$BsmtFullBath <- as.factor(all_data$BsmtFullBath) 
all_data$BsmtHalfBath <- as.factor(all_data$BsmtHalfBath)
all_data$FullBath <- as.factor(all_data$FullBath)
all_data$HalfBath <- as.factor(all_data$HalfBath)
all_data$BedroomAbvGr <- as.factor(all_data$BedroomAbvGr)
all_data$KitchenAbvGr <- as.factor(all_data$KitchenAbvGr)
all_data$Fireplaces <- as.factor(all_data$Fireplaces)
all_data$GarageCars <- as.factor(all_data$GarageCars)
all_data$YrSold <- as.factor(all_data$YrSold)

all_data$YearBuilt <- 2019 - all_data$YearBuilt
all_data$YearRemodAdd <- 2019 - all_data$YearRemodAdd
all_data$GarageYrBlt <- 2019 - all_data$GarageYrBlt


str(all_data)
#na 치환  
all_data$LotFrontage[is.na(all_data$LotFrontage)] <- mean(all_data$LotFrontage,na.rm=TRUE)

all_data$SalePrice <- as.integer(all_data$SalePrice)
all_data$YearBuilt <- as.integer(all_data$YearBuilt)                                                                                                  
all_data$YearRemodAdd <- as.integer(all_data$YearRemodAdd)
all_data$GarageYrBlt <- as.integer(all_data$GarageYrBlt)    
all_data$LotFrontage <- as.integer(all_data$LotFrontage) 

int <- all_data[,sapply(all_data,is.integer)]
chr <- all_data[,sapply(all_data,is.factor)]  
#str(int)
#str(chr)

chr <- as.matrix(chr)
int[is.na(int)] <- 1
chr[is.na(chr)] <- "na"
#sum(is.na(int))
#sum(is.na(chr))
chr <- as.data.frame(chr)
int <- as.data.frame(scale(int))
all_data <- cbind(int,chr,SalePrice)   

str(all_data)

train <- all_data[1:1460,]
test <- all_data[1461:2919,]

#standart scale
train_int <- train[,sapply(train,is.integer)]
train_fac <- train[,sapply(train,is.factor)]
train_int
train <- cbind(as.data.frame(scale(train_int)), train_fac)

#na 비율확인
names(train[is.na(train),])
train %>%
  group_by(Alley) %>%
  summarize(n = n()) 
str(train$Alley)

str(train)
#Alley,FireplaceQu,PoolQC,Fence,MiscFeature


#이상치 확인

boxplot(train$LotFrontage)
#boxplot(train$LotArea)$stats[1,]
#boxplot(train$LotArea)$stats[5,]
train$LotFrontage <- ifelse(train$LotFrontage < boxplot(train$LotFrontage)$stats[1,],boxplot(train$LotFrontage)$stats[1,],train$LotFrontage)
train$LotFrontage <- ifelse(train$LotFrontage > boxplot(train$LotFrontage)$stats[5,],boxplot(train$LotFrontage)$stats[5,],train$LotFrontage)
summary(train$LotFrontage)

boxplot(train$LotArea)
train$LotArea <- ifelse(train$LotArea < boxplot(train$LotArea)$stats[1,],boxplot(train$LotArea)$stats[1,],train$LotArea)
train$LotArea <- ifelse(train$LotArea > boxplot(train$LotArea)$stats[5,],boxplot(train$LotArea)$stats[5,],train$LotArea)
summary(train$LotArea)

boxplot(train$GrLivArea)
train$GrLivArea <- ifelse(train$GrLivArea < boxplot(train$GrLivArea)$stats[1,],boxplot(train$GrLivArea)$stats[1,],train$GrLivArea)
train$GrLivArea <- ifelse(train$GrLivArea > boxplot(train$GrLivArea)$stats[5,],boxplot(train$GrLivArea)$stats[5,],train$GrLivArea)
summary(train$GrLivArea)

boxplot(train$TotalBsmtSF)
train$TotalBsmtSF <- ifelse(train$TotalBsmtSF < boxplot(train$TotalBsmtSF)$stats[1,],boxplot(train$TotalBsmtSF)$stats[1,],train$TotalBsmtSF)
train$TotalBsmtSF <- ifelse(train$TotalBsmtSF > boxplot(train$TotalBsmtSF)$stats[5,],boxplot(train$TotalBsmtSF)$stats[5,],train$TotalBsmtSF)
summary(train$TotalBsmtSF)

boxplot(train$X1stFlrSF)
train$X1stFlrSF <- ifelse(train$X1stFlrSF < boxplot(train$X1stFlrSF)$stats[1,],boxplot(train$X1stFlrSF)$stats[1,],train$X1stFlrSF)
train$X1stFlrSF <- ifelse(train$X1stFlrSF > boxplot(train$X1stFlrSF)$stats[5,],boxplot(train$X1stFlrSF)$stats[5,],train$X1stFlrSF)
summary(train$X1stFlrSF)

boxplot(train$X2ndFlrSF)
train$X2ndFlrSF <- ifelse(train$X2ndFlrSF < boxplot(train$X2ndFlrSF)$stats[1,],boxplot(train$X2ndFlrSF)$stats[1,],train$X2ndFlrSF)
train$X2ndFlrSF <- ifelse(train$X2ndFlrSF > boxplot(train$X2ndFlrSF)$stats[5,],boxplot(train$X2ndFlrSF)$stats[5,],train$X2ndFlrSF)
summary(train$X2ndFlrSF)


boxplot(train$GarageArea)
train$GarageArea <- ifelse(train$GarageArea < boxplot(train$GarageArea)$stats[1,],boxplot(train$GarageArea)$stats[1,],train$GarageArea)
train$GarageArea <- ifelse(train$GarageArea > boxplot(train$GarageArea)$stats[5,],boxplot(train$GarageArea)$stats[5,],train$GarageArea)
summary(train$X1stFlrSF)


boxplot(train$LotArea)
train$LotArea <- ifelse(train$LotArea < boxplot(train$LotArea)$stats[1,],boxplot(train$LotArea)$stats[1,],train$LotArea)

boxplot(train$BsmtFinSF1)
train$BsmtFinSF1 <- ifelse(train$BsmtFinSF1 < boxplot(train$BsmtFinSF1)$stats[1,],boxplot(train$BsmtFinSF1)$stats[1,],train$BsmtFinSF1)
train$BsmtFinSF1 <- ifelse(train$BsmtFinSF1 > boxplot(train$BsmtFinSF1)$stats[5,],boxplot(train$BsmtFinSF1)$stats[5,],train$BsmtFinSF1)
summary(train$BsmtFinSF1)

boxplot(train$BsmtUnfSF)
train$BsmtUnfSF <- ifelse(train$BsmtUnfSF < boxplot(train$BsmtUnfSF)$stats[1,],boxplot(train$BsmtUnfSF)$stats[1,],train$BsmtUnfSF)
train$BsmtUnfSF <- ifelse(train$BsmtUnfSF > boxplot(train$BsmtUnfSF)$stats[5,],boxplot(train$BsmtUnfSF)$stats[5,],train$BsmtUnfSF)
summary(train$BsmtUnfSF)

boxplot(train$MasVnrArea)
train$MasVnrArea <- ifelse(train$MasVnrArea < boxplot(train$MasVnrArea)$stats[1,],boxplot(train$MasVnrArea)$stats[1,],train$MasVnrArea)
train$MasVnrArea <- ifelse(train$MasVnrArea > boxplot(train$MasVnrArea)$stats[5,],boxplot(train$MasVnrArea)$stats[5,],train$MasVnrArea)
summary(train$MasVnrArea)

#TotalBsmtSF,X1stFlrSF - 공선성
#GarageArea,LotArea,BsmtFinSF1,BsmtUnfSF,MasVnrArea




#write.csv(train,file = "train_preprocess_1.csv" )

####################################################################################################################################
# step_02-2
#RandomForest 인자찾기(y- linear)
library(caret)
####################################################################################################################################
#write.csv(train,file = "train_exp_1.csv" )
str(train)
#train <- train[,-c(1:2)]
#train <- train[,-c(1)]
#hist(train$SalePrice)

set.seed(777)
train_idx <- sample(seq_len(nrow(train)),floor(nrow(train)*0.8))
#train_idx
p_train <- train[train_idx,]
p_test <- train[-train_idx,]

str(p_train)
#train_rf <- randomForest(SalePrice_class~.,data=train)
train_rf <- randomForest(SalePrice~.,data=p_train)
train_rf
train_rf$importance
varImpPlot(train_rf)

#검증
#정확도(train)
rf_pr_train <- predict(train_rf,newdata=p_train)
#rf_pr_train
actual <- p_train$SalePrice
preds <- rf_pr_train
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq
plot(preds,actual)

#정확도(test)
rf_pr_test <- predict(train_rf,newdata=p_test)
#rf_pr_test
actual <- p_test$SalePrice
preds <- rf_pr_test
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq
plot(preds,actual)



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
p_train2 <- as.data.frame(p_train2)
#데이터 셋 구성
x_train <- model.matrix(SalePrice ~., p_train2)[,-1]
y_train <- p_train2$SalePrice
x_test<-model.matrix(SalePrice~., p_test2)[,-1]
y_test<- p_test2$SalePrice


lm_price<-lm(formula = SalePrice ~ . , data = p_train)
summary(lm_price)

#검증
pred_te_lm <- predict(lm_price,  newdata = p_test)
actual <- y_test
preds <- pred_tr_lm
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq




#########################################################################################
#lasso

#데이터 셋 구성
x_train <- model.matrix(SalePrice ~., p_train)[,-1]
y_train <- p_train$SalePrice
x_test<-model.matrix(SalePrice~., p_test)[,-1]
y_test<- p_test$SalePrice

lambda_seq <- 100^seq(2, -2, by = -.005)
#lambda_seq
cv.lasso <- cv.glmnet(x_train, y_train, alpha=1, lambda = lambda_seq)
plot(cv.lasso)
cv.lasso
best_lam <- cv.lasso$lambda.1se
best_lam <- cv.lasso$lambda.min
best_lam

lasso_best <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam)
lasso_best

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
seq<-c(1:402)
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
