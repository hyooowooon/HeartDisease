# heartDisease 

heart <- read.csv(file.choose(), stringsAsFactors = F)
str(heart)
# 'data.frame':	303 obs. of  15 variables:
# $ id      : int  1 2 3 4 5 6 7 8 9 10 ...
# $ age     : int  63 37 41 56 57 57 56 44 52 57 ...
# $ sex     : int  1 1 0 1 0 1 0 1 1 1 ...
# $ cp      : int  3 2 1 1 0 0 1 1 2 2 ...
# $ trtbps  : int  145 130 130 120 120 140 140 120 172 150 ...
# $ chol    : int  233 250 204 236 354 192 294 263 199 168 ...
# $ fbs     : int  1 0 0 0 0 0 0 0 1 0 ...
# $ restecg : int  0 1 0 1 1 1 0 1 1 1 ...
# $ thalachh: int  150 187 172 178 163 148 153 173 162 174 ...
# $ exng    : int  0 0 0 0 1 0 0 0 0 0 ...
# $ oldpeak : num  2.3 3.5 1.4 0.8 0.6 0.4 1.3 0 0.5 1.6 ...
# $ slp     : int  0 0 2 2 2 1 1 2 2 2 ...
# $ caa     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ thall   : int  1 2 2 2 2 1 2 3 3 2 ...
# $ output  : int  1 1 1 1 1 1 1 1 1 1 ...

table(is.na(heart)) 
# FALSE 
# 4545 

# 발병여부 빈도수&비율
table(heart$output)
#   0   1 
# 138 165
prop.table(table(heart$output))
#         0         1 
# 0.4554455 0.5445545 

#-----------------------------------------------------------
# 상관분석

# 필요한 패키지 설치
install.packages("corrgram")
install.packages("PerformanceAnalytics") 
install.packages("corrplot")
install.packages("ggplot")
library(corrgram)
library(PerformanceAnalytics) 
library(corrplot)
library(ggplot2)

# id 변수 제외한 데이터셋 만들기
heart_1 <- heart[c(-1)]
str(heart_1)
COR <- cor(heart_1)
COR['output', ]
#         age         sex          cp      trtbps        chol 
# -0.22543872 -0.28093658  0.43379826 -0.14493113 -0.08523911 
#         fbs     restecg    thalachh        exng     oldpeak 
# -0.02804576  0.13722950  0.42174093 -0.43675708 -0.43069600 
#        slp         caa       thall      output 
# 0.34587708 -0.39172399 -0.34402927  1.00000000 

corrplot(COR)
corrgram(heart_1)
corrgram(heart_1, upper.panel=panel.conf) 
corrgram(heart_1, lower.panel=panel.conf) 
chart.Correlation(heart_1, histogram=TRUE) 


#------------------------------------------------------------

# 회귀모델 생성
library(MASS)
m <- lm(output~., data=heart_1)
summary(m)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.8288987  0.2929344   2.830 0.004987 ** 
# age         -0.0008204  0.0026962  -0.304 0.761129  > 0.05  
# sex         -0.1959956  0.0471429  -4.157 4.24e-05 ***
# cp           0.1127034  0.0223816   5.036 8.40e-07 ***
# trtbps      -0.0019910  0.0012573  -1.583 0.114407    
# chol        -0.0003535  0.0004217  -0.838 0.402545    
# fbs          0.0173736  0.0596669   0.291 0.771125  > 0.05
# restecg      0.0498480  0.0399228   1.249 0.212819    
# thalachh     0.0030193  0.0011304   2.671 0.007988 ** 
# exng        -0.1440459  0.0513689  -2.804 0.005387 ** 
# oldpeak     -0.0587887  0.0229269  -2.564 0.010847 *  
# slp          0.0789788  0.0423896   1.863 0.063453 . > 0.05
# caa         -0.1006022  0.0218565  -4.603 6.25e-06 ***
# thall       -0.1190392  0.0356550  -3.339 0.000952 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3542 on 289 degrees of freedom
# Multiple R-squared:  0.5175,	Adjusted R-squared:  0.4958 
# F-statistic: 23.85 on 13 and 289 DF,  p-value: < 2.2e-16


# 변수 선택법 
m2 <- step(m, direction = "both")


# leaps 패키지 이용 -> 최종 변수 선택
install.packages("leaps")
library(leaps)
m3 <- regsubsets(output~., data = heart_1)
summary(m3)

# 변수 2개 선택 시 :
# age sex cp  trtbps chol fbs restecg thalachh exng oldpeak slp caa thall
# " " " " "*" " "    " "  " " " "     " "      " "  "*"     " " " " " "  

# 변수 4개 선택 시:
# age sex cp  trtbps chol fbs restecg thalachh exng oldpeak slp caa thall
# " " "*" "*" " "    " "  " " " "     " "      " "  "*"     " " "*" " " 

# 변수 6개 선택 시:
# age sex cp  trtbps chol fbs restecg thalachh exng oldpeak slp caa thall
# " " "*" "*" " "    " "  " " " "     "*"      " "  "*"     " " "*" "*"  

# 변수 8개 선택 시:
# age sex cp  trtbps chol fbs restecg thalachh exng oldpeak slp caa thall
# " " "*" "*" "*"    " "  " " " "     "*"      "*"  "*"     " " "*" "*"  


# 다중회귀분석 
# 1. 모델의 유의성 검정 : F-statistic
# 2. 모델의 설명력 : Adjusted R-squared
# 3. x의 유의성 검정 : t value Pr(>|t|)

# 변수 2개
y = heart_1$output
x1 = heart_1$cp
x2 = heart_1$oldpeak

df2 <- data.frame(x1, x2, y)
result.lm <- lm(formula=y ~ ., data=df2)
result.lm

summary(result.lm)
# Residual standard error: 0.4112 on 300 degrees of freedom
# Multiple R-squared:  0.3252,	Adjusted R-squared:  0.3207 
# F-statistic: 72.27 on 2 and 300 DF,  p-value: < 2.2e-16

# 변수 4개
y = heart_1$output
x1 = heart_1$sex
x2 = heart_1$cp
x3 = heart_1$oldpeak
x4 = heart_1$caa

df4 <- data.frame(x1, x2, x3, x4, y)
result.lm <- lm(formula=y ~ ., data=df4)
summary(result.lm)
# Residual standard error: 0.3794 on 298 degrees of freedom
# Multiple R-squared:  0.4291,	Adjusted R-squared:  0.4214 
# F-statistic: 55.99 on 4 and 298 DF,  p-value: < 2.2e-16


# 변수 6개
y = heart_1$output
x1 = heart_1$sex
x2 = heart_1$cp
x3 = heart_1$thalachh
x4 = heart_1$oldpeak
x5 = heart_1$caa
x6 = heart_1$thall

df6 <- data.frame(x1, x2, x3, x4, x5, x6, y)
result.lm <- lm(formula=y ~ ., data=df6)
summary(result.lm)

# Residual standard error: 0.3619 on 296 degrees of freedom
# Multiple R-squared:  0.4841,	Adjusted R-squared:  0.4737 
# F-statistic:  46.3 on 6 and 296 DF,  p-value: < 2.2e-16

# 변수 8개
y = heart_1$output
x1 = heart_1$sex
x2 = heart_1$cp
x3 = heart_1$trtbps
x4 = heart_1$thalachh
x5 = heart_1$exng
x6 = heart_1$oldpeak
x7 = heart_1$caa
x8 = heart_1$thall

df8 <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, y)
result.lm <- lm(formula=y ~ ., data=df8)
summary(result.lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  0.822111   0.231799   3.547 0.000454 ***
#   x1          -0.185207   0.045500  -4.070 6.04e-05 ***
#   x2           0.113645   0.022230   5.112 5.75e-07 ***
#   x3          -0.002338   0.001202  -1.945 0.052695 .  
#   x4           0.003564   0.001023   3.482 0.000573 ***
#   x5          -0.152103   0.051126  -2.975 0.003172 ** 
#   x6          -0.081109   0.019807  -4.095 5.46e-05 ***
#   x7          -0.099669   0.021138  -4.715 3.73e-06 ***
#   x8          -0.120933   0.035435  -3.413 0.000733 ***

# Residual standard error: 0.3552 on 294 degrees of freedom
# Multiple R-squared:  0.5065,	Adjusted R-squared:  0.4931 
# F-statistic: 37.72 on 8 and 294 DF,  p-value: < 2.2e-16

# 다중회귀분석 회귀선 시각화
library(psych)
pairs.panels(df8, stars = TRUE, lm = TRUE, ci = TRUE)

# ------------------------------------------------------------------
# 랜덤포레스트

# 종속변수 : 숫자형->범주형 전환 
heart$output <- as.factor(heart$output)

heart_2 <- heart[-c(1:2, 6:8, 12)]
str(heart_2)

# - 훈련셋 : heart_train, 검정셋 : heart_test    
idx = sample(nrow(heart_2), 0.7*nrow(heart_2))
heart_train = heart_2[idx, ] # 훈련 데이터 
heart_test = heart_2[-idx, ] # 검정 데이터 
dim(heart_train) # 212  9
dim(heart_test) # 91  9

# 분류모델 생성 : 종속변수=output, 독립변수=나머지 변수  
library(rpart)
model <- rpart(output ~ ., data = heart_train)
model 

# 의사결정트리 시각화 : 중요변수 2~3개를 기준으로 발병확률이 높은 경우 설명하기   
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model)

#----------------------------------------------------

library(MASS)
library(randomForest)
library(caret)


heart_model <- randomForest(y ~ ., data = df8,
                             mtree = 500, mtry = 8,
                             importance = T,
                             na.action=na.omit)
heart_model


# 중요변수 확인
importance(heart_model)
varImpPlot(heart_model)

#      %IncMSE IncNodePurity
# x1 15.336041      2.202831
# x2 27.484059     15.936257
# x3 -1.168272      6.675140
# x4 10.816712     10.154444
# x5  6.924433      2.679695
# x6 26.195388     10.765246
# x7 32.469807     10.708037
# x8 21.305646     10.111440


rf_p <- predict(model, newdata = heart_test, type = "class")
confusionMatrix(rf_p, heart_test$output)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1
# 0 27  4
# 1  9 51
# 
# Accuracy : 0.8571          
# 95% CI : (0.7681, 0.9217)
# No Information Rate : 0.6044          
# P-Value [Acc > NIR] : 1.294e-07       
# 
# Kappa : 0.6939          
# 
# Mcnemar's Test P-Value : 0.2673          
#                                           
#             Sensitivity : 0.7500          
#             Specificity : 0.9273          
#          Pos Pred Value : 0.8710          
#          Neg Pred Value : 0.8500          
#              Prevalence : 0.3956          
#          Detection Rate : 0.2967          
#    Detection Prevalence : 0.3407          
#       Balanced Accuracy : 0.8386          
#                                           
#        'Positive' Class : 0           


