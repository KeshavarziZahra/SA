
rm(list=ls(all=TRUE))
##############################################################################
###########################  Install Package #################################

#install.packages("C50")
#install.packages("pROC")
#install.packages("RSNNS")
#install.packages("rpart")
#install.packages("readxl")
#install.packages("writexl")
#install.packages("BayesTree")
#install.packages("rpart.plot")


library(C50)
library(pROC)
library(RSNNS)
library(rpart)
library(readxl)
library(writexl)
library(BayesTree)
library(rpart.plot)

###########################
###########################  Open Data 

a <- file.choose()
Data <- read_xlsx(a)
Data <- data.frame(Data)
head(Data)

L <- which(Data[, 7] == -1)
Data[L, 7] <- 0
T <- table(Data[, 1])
length(which(T > 1))
Name <- names(T[which(T > 1)])
L <- c()
for(i in 1:length(Name)){
	L1 <- which(Data[, 1] == Name[i])
	L <- c(L, L1[- 1])
}
L <- sort(L)
Data2 <- Data[- L, ]
write_xlsx(Data2, "SPSS.xlsx")
SPSS <- Data2
Data2 <- Data2[, 2:7]

###########################
###########################  Test & Train Data

P <- 0.8
n <- nrow(Data2)
L <- 1:n
n1 <- trunc(P * n)
n2 <- n - n1
L1 <- sample(L, n1)
L1 <- sort(L1)
L2 <- L[ - L1]
Train <- Data2[L1, ]
Test <- Data2[L2, ]
write_xlsx(Test, "Test.xlsx")
write_xlsx(Train, "Train.xlsx")
x_test <- Test[, 1:5]
y_test <- Test[, 6]
x_train <- Train[, 1:5]
y_train <- Train[, 6]

###########################
###########################  GLM Model Logistic Regression

Model1 <- glm(Y ~ ., family = binomial(link = "logit"), data = Train)
summary(Model1)
Model1 <- glm(Y ~ -1 + x1 + x2 + x3 + x4 + x5,
family = binomial(link = "logit"), data = Train)
summary(Model1)

yhat_Test_1 <- predict(Model1, newdata = x_test, type = "response")
for(i in 1:length(yhat_Test_1))	yhat_Test_1[i] <- rbinom(1, 1, yhat_Test_1[i])
yhat_Train_1 <- predict(Model1, newdata = x_train, type = "response")
for(i in 1:length(yhat_Train_1))	yhat_Train_1[i] <- rbinom(1, 1, yhat_Train_1[i])

T_Test_1 <- table(y_test, yhat_Test_1)
T_Train_1 <- table(y_train, yhat_Train_1)

AC_Test_1 <- (T_Test_1[1, 1] + T_Test_1[2, 2]) / sum(T_Test_1)
AC_Train_1 <- (T_Train_1[1, 1] + T_Train_1[2, 2]) / sum(T_Train_1)

TN_Test_1 <- T_Test_1[1, 1] / (T_Test_1[1, 1] +T_Test_1[1, 2])
TN_Train_1 <- T_Train_1[1, 1] / (T_Train_1[1, 1] +T_Train_1[1, 2])

TP_Test_1 <- T_Test_1[2, 2] / (T_Test_1[2, 1] +T_Test_1[2, 2])
TP_Train_1 <- T_Train_1[2, 2] / (T_Train_1[2, 1] +T_Train_1[2, 2])

Roc_Test_1 <- roc(y_test, yhat_Test_1)
Roc_Train_1 <- roc(y_train, yhat_Train_1) 

Auc_Test_1 <- auc(Roc_Test_1)
Auc_Train_1 <- auc(Roc_Train_1)

plot(Roc_Test_1, col = 2, lwd = 3, main = "Roc Curve For Test Data")
plot(Roc_Train_1, col = 3, lwd = 3, main = "Roc Curve For Train Data")

###########################
###########################  rpart

Model2 <- rpart( Y ~ ., data = Train, method = "class")
rpart.plot(Model2)

yhat_Test_2 <- predict(Model2, newdata = x_test)
yhat_Test_2 <- yhat_Test_2[, 2]
for(i in 1:length(yhat_Test_2))	yhat_Test_2[i] <- rbinom(1, 1, yhat_Test_2[i])

yhat_Train_2 <- predict(Model2, newdata = x_train)
yhat_Train_2 <- yhat_Train_2[, 2]
for(i in 1:length(yhat_Train_2))	yhat_Train_2[i] <- rbinom(1, 1, yhat_Train_2[i])

T_Test_2 <- table(y_test, yhat_Test_2)
T_Train_2 <- table(y_train, yhat_Train_2)

AC_Test_2 <- (T_Test_2[1, 1] + T_Test_2[2, 2]) / sum(T_Test_2)
AC_Train_2 <- (T_Train_2[1, 1] + T_Train_2[2, 2]) / sum(T_Train_2)

TN_Test_2 <- T_Test_2[1, 1] / (T_Test_2[1, 1] +T_Test_2[1, 2])
TN_Train_2 <- T_Train_2[1, 1] / (T_Train_2[1, 1] +T_Train_2[1, 2])

TP_Test_2 <- T_Test_2[2, 2] / (T_Test_2[2, 1] +T_Test_2[2, 2])
TP_Train_2 <- T_Train_2[2, 2] / (T_Train_2[2, 1] +T_Train_2[2, 2])

Roc_Test_2 <- roc(y_test, yhat_Test_2)
Roc_Train_2 <- roc(y_train, yhat_Train_2) 

Auc_Test_2 <- auc(Roc_Test_2)
Auc_Train_2 <- auc(Roc_Train_2)

plot(Roc_Test_2, col = 2, lwd = 3, main = "Roc Curve For Test Data")
plot(Roc_Train_2, col = 3, lwd = 3, main = "Roc Curve For Train Data")

###########################
###########################  C50

Model3 <- C5.0(x = x_train, y = factor(y_train))
plot(Model3)

yhat_Test_3 <- predict(Model3, newdata = x_test)
yhat_Train_3 <- predict(Model3, newdata = x_train)

T_Test_3 <- table(y_test, yhat_Test_3)
T_Train_3 <- table(y_train, yhat_Train_3)

AC_Test_3 <- (T_Test_3[1, 1] + T_Test_3[2, 2]) / sum(T_Test_3)
AC_Train_3 <- (T_Train_3[1, 1] + T_Train_3[2, 2]) / sum(T_Train_3)

TN_Test_3 <- T_Test_3[1, 1] / (T_Test_3[1, 1] +T_Test_3[1, 2])
TN_Train_3 <- T_Train_3[1, 1] / (T_Train_3[1, 1] +T_Train_3[1, 2])

TP_Test_3 <- T_Test_3[2, 2] / (T_Test_3[2, 1] +T_Test_3[2, 2])
TP_Train_3 <- T_Train_3[2, 2] / (T_Train_3[2, 1] +T_Train_3[2, 2])

Roc_Test_3 <- roc(y_test, as.numeric(as.vector(yhat_Test_3)))
Roc_Train_3 <- roc(y_train, as.numeric(as.vector(yhat_Train_3)))

Auc_Test_3 <- auc(Roc_Test_3)
Auc_Train_3 <- auc(Roc_Train_3)

plot(Roc_Test_3, col = 2, lwd = 3, main = "Roc Curve For Test Data")
plot(Roc_Train_3, col = 3, lwd = 3, main = "Roc Curve For Train Data")

###########################
###########################  BayesTree

Model4 <- bart(x.train = x_train, y.train = y_train, x.test = x_test)
yhat_Test_4 <- pnorm(apply(Model4 $  yhat.test, 2, mean))
for(i in 1:length(yhat_Test_4))	yhat_Test_4[i] <- rbinom(1, 1, yhat_Test_4[i])
yhat_Train_4 <- pnorm(apply(Model4 $  yhat.train, 2, mean))
for(i in 1:length(yhat_Train_4))	yhat_Train_4[i] <- rbinom(1, 1, yhat_Train_4[i])

T_Test_4 <- table(y_test, yhat_Test_4)
T_Train_4 <- table(y_train, yhat_Train_4)
 
AC_Test_4 <- (T_Test_4[1, 1] + T_Test_4[2, 2]) / sum(T_Test_4)
AC_Train_4 <- (T_Train_4[1, 1] + T_Train_4[2, 2]) / sum(T_Train_4)

TN_Test_4 <- T_Test_4[1, 1] / (T_Test_4[1, 1] +T_Test_4[1, 2])
TN_Train_4 <- T_Train_4[1, 1] / (T_Train_4[1, 1] +T_Train_4[1, 2])

TP_Test_4 <- T_Test_4[2, 2] / (T_Test_4[2, 1] +T_Test_4[2, 2])
TP_Train_4 <- T_Train_4[2, 2] / (T_Train_4[2, 1] +T_Train_4[2, 2])

Roc_Test_4 <- roc(y_test, yhat_Test_4)
Roc_Train_4 <- roc(y_train, yhat_Train_4) 

Auc_Test_4 <- auc(Roc_Test_4)
Auc_Train_4 <- auc(Roc_Train_4)

plot(Roc_Test_4, col = 2, lwd = 3, main = "Roc Curve For Test Data")
plot(Roc_Train_4, col = 3, lwd = 3, main = "Roc Curve For Train Data")

###########################
###########################  RBF

Model5 <- rbf(x = x_train, y = y_train, size=40, maxit=1000)
summary(Model5)
yhat_Test_5 <- sign(predict(Model5, newdata = x_test))
L <- which(yhat_Test_5 == -1)
yhat_Test_5[L] <- 0

yhat_Train_5 <- sign(predict(Model5, newdata = x_train))
L <- which(yhat_Train_5 == -1)
yhat_Train_5[L] <- 0

T_Test_5 <- table(y_test, yhat_Test_5)
T_Train_5 <- table(y_train, yhat_Train_5)

AC_Test_5 <- (T_Test_5[1, 1] + T_Test_5[2, 2]) / sum(T_Test_5)
AC_Train_5 <- (T_Train_5[1, 1] + T_Train_5[2, 2]) / sum(T_Train_5)

TN_Test_5 <- T_Test_5[1, 1] / (T_Test_5[1, 1] +T_Test_5[1, 2])
TN_Train_5 <- T_Train_5[1, 1] / (T_Train_5[1, 1] +T_Train_5[1, 2])

TP_Test_5 <- T_Test_5[2, 2] / (T_Test_5[2, 1] +T_Test_5[2, 2])
TP_Train_5 <- T_Train_5[2, 2] / (T_Train_5[2, 1] +T_Train_5[2, 2])

Roc_Test_5 <- roc(y_test, as.numeric(as.vector(yhat_Test_5)))
Roc_Train_5 <- roc(y_train, as.numeric(as.vector(yhat_Train_5)))

Auc_Test_5 <- auc(Roc_Test_5)
Auc_Train_5 <- auc(Roc_Train_5)

plot(Roc_Test_5, col = 2, lwd = 3, main = "Roc Curve For Test Data")
plot(Roc_Train_5, col = 3, lwd = 3, main = "Roc Curve For Train Data")

###########################
###########################  Accuracy

AC <- matrix(c(
AC_Train_1, AC_Test_1,
AC_Train_2, AC_Test_2,
AC_Train_3, AC_Test_3,
AC_Train_4, AC_Test_4,
AC_Train_5, AC_Test_5), nc = 2, byrow = TRUE)
colnames(AC) <- c("AC_Train", "AC_Test")
rownames(AC) <- c("Logistic_Regression", "Classification_Tree", "C5.0_Tree",
"Bayes_Tree", "RBF_Network")
AC <- round(AC, 3)

TN <- matrix(c(
TN_Train_1, TN_Test_1,
TN_Train_2, TN_Test_2,
TN_Train_3, TN_Test_3,
TN_Train_4, TN_Test_4,
TN_Train_5, TN_Test_5), nc = 2, byrow = TRUE)
colnames(TN) <- c("TN_Train", "TN_Test")
rownames(TN) <- c("Logistic_Regression", "Classification_Tree", "C5.0_Tree",
"Bayes_Tree", "RBF_Network")
TN <- round(TN, 3)

TP <- matrix(c(
TP_Train_1, TP_Test_1,
TP_Train_2, TP_Test_2,
TP_Train_3, TP_Test_3,
TP_Train_4, TP_Test_4,
TP_Train_5, TP_Test_5), nc = 2, byrow = TRUE)
colnames(TP) <- c("TP_Train", "TP_Test")
rownames(TP) <- c("Logistic_Regression", "Classification_Tree", "C5.0_Tree",
"Bayes_Tree", "RBF_Network")
TP <- round(TP, 3)

Auc <- matrix(c(
Auc_Train_1[[1]], Auc_Test_1[[1]],
Auc_Train_2[[1]], Auc_Test_2[[1]],
Auc_Train_3[[1]], Auc_Test_3[[1]],
Auc_Train_4[[1]], Auc_Test_4[[1]],
Auc_Train_5[[1]], Auc_Test_5[[1]]), nc = 2, byrow = TRUE)
colnames(Auc) <- c("Auc_Train", "Auc_Test")
rownames(Auc) <- c("Logistic_Regression", "Classification_Tree", "C5.0_Tree",
"Bayes_Tree", "RBF_Network")
Auc <- round(Auc, 3)

###########################
###########################  T Test

head(SPSS)
ID <- SPSS[, 1]
z1 <- SPSS[, 2]
z2 <- SPSS[, 3]
z3 <- SPSS[, 4]
z4 <- SPSS[, 5]
z5 <- SPSS[, 6]
Y <- SPSS[, 7]
score <- SPSS[, 8]
NPS <- SPSS[, 9] 

table(z1)
table(z2)
table(z3)
table(z4)
table(z5)

L <- which(z1 > 2)
z1[L] <- 3

L <- which(z2 > 2)
z2[L] <- 3

L <- which(z3 > 2)
z3[L] <- 3

L <- which(z4 > 2)
z4[L] <- 3

L <- which(z5 > 2)
z5[L] <- 3

Anova <- data.frame(z1, z2, z3, z4, z5, Y, score, NPS)
write_xlsx(Anova, "Anova.xlsx")

