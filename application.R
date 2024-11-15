# Required Libraries
library(rattle)
library(lubridate)
library(SPEI)
library(sm)
library(robustbase)
library(MASS)
library(mvtnorm)
library(GGally)
library(lmtest)
library(nortest)

################### Loading data set ###################
data("weatherAUS")
data <- weatherAUS

########################################################################
######################### Preprocessing ################################
########################################################################

data <- data[42932:47584,] #selecting Sydney
data <- na.omit(data) #excluing NA's

# Aggregating Day/Month/Year
d <- day(as.data.frame(data[,1])[,])
m <- month(as.data.frame(data[,1])[,])
y <- year(as.data.frame(data[,1])[,])
data <- cbind(y, m, d,data[c(-1)])

# Selecting January
data <- data[data$m == "1",]
nrow(data) #check (246)
attach(data)

# Dataset - Phase 1
data_phase1 <- data[1:123,]

# Dataset - Phase 2
data_phase2 <- data[124:246,]

########################################################################
################# Regression Analysis Procedure ########################
########################################################################

# Fitting the final linear model (after variable selection)
model = lm(Temp3pm ~ Rainfall + Sunshine  + WindGustSpeed + 
                 WindSpeed3pm + Humidity3pm + Pressure3pm, data = data_phase1) 
summary(model) #0.6453

# Residuals and Diagnostic
res_model = residuals(model)
fit_model =  fitted.values(model)

# Standarized residuals
ard = ls.diag(model)
std_residual = ard$std.res

# Some residuals plots 
# Outliers
plot(std_residual)
abline(h=-3, col = 2); abline(h=3, col=2)
which(abs(std_residual)>3)

#Homoscedasticity
plot(fit_model, std_residual, main="Homoscedasticity: residuals vs fitted values",
     ylab = "standardized residuals", xlab = "fitted values")
#Goldfeld-Quandt test
gqtest(model, alternative = "two.sided")

#QQ Plot
qqnorm(std_residual, main = "QQ Plot"); qqline(std_residual, col=2)
lillie.test(std_residual)

#Linearity
plot(fit_model, Temp3pm[1:123],main="Linearity",
     ylab = "Temperature", xlab = "fitted values")

# ACF plot
acf(std_residual, main="")

########################################################################
########################### ROBUST CONTROL CHARTS ######################
########################################################################

# alpha
alpha <- 0.01

################# RCC and RCC_MAD Control Charts ######################
# Fits the OLS regression model on Phase 1
model_ols = lm(Temp3pm ~ Rainfall + Sunshine  + WindGustSpeed +
                  WindSpeed3pm + Humidity3pm + Pressure3pm, data = data_phase1)
# Store
sum <- summary(model_ols)
coeff <- as.numeric(model_ols$coefficients)

# Selecting sigma and MAD to RCC Chart
sd_model <- sum$sigma #sigma
sd_model_mad <- mad(residuals(model_ols)) #MAD

# Select the X's from Phase 2
X_phase2 <- cbind(data_phase2$Rainfall,data_phase2$Sunshine,
                     data_phase2$WindGustSpeed,data_phase2$WindSpeed3pm,
                     data_phase2$Humidity3pm,data_phase2$Pressure3pm)

# Computing y hat on Phase 2
mu_hat <- cbind(1,X_phase2)%*%coeff

# RCC Control limits on Phase 2
LCL <- qnorm(alpha/2, mu_hat, sd_model)#; LIC
UCL <- qnorm(1 - (alpha/2), mu_hat, sd_model)#; LSC

# RCC_MAD Control limits on Phase 2
LIC_mad <- qnorm(alpha/2, mu_hat, sd_model_mad)#; LIC
LSC_mad <- qnorm(1 - (alpha/2), mu_hat, sd_model_mad)#; LSC

# Plotting RCC Control Chart (Phase 2)
plot(data_phase2$Temp3pm, col=ifelse(data_phase2$Temp3pm>UCL, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCL, 16, 1),
     main= "RCC", ylim=c(10,45), ylab= "Temperature", xlab="Observations (Phase 2)")
lines(x=1:123,y=LCL, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCL, lty = 1,lwd = 1, col = 'red')

# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCL) # integer(0)
which(abs(data_phase2$Temp3pm)>UCL) # 30  53 102

# Plotting RCC_MAD Control Chart (Phase 2)
plot(data_phase2$Temp3pm, col=ifelse(data_phase2$Temp3pm>UCL_mad, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCL_mad, 16, 1),
     main= "RCC mad",ylim=c(10,45), ylab= "Temperature", xlab="Observations (phase 2)")
lines(x=1:123,y=LCL_mad, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCL_mad, lty = 1,lwd = 1, col = 'red')

# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCL_mad) # integer(0)
which(abs(data_phase2$Temp3pm)>UCL_mad) # 30  53  91  98 102 103 106

############## M-Regression Control Chart ################################
# Fits the M-regression model on Phase 1
mod_M = rlm(Temp3pm ~ Rainfall + Sunshine  + WindGustSpeed +
              WindSpeed3pm + Humidity3pm + Pressure3pm, data = data_phase1)
# Store
coeff_M <- as.numeric(mod_M$coefficients)
# Selecting sigma
sigma_m = mod_M$s

# Computing y hat on Phase 2
mu_hat_M <- cbind(1,X_phase2)%*%coeff_M

# MRCC Control limits on Phase 2 
LCLM <- qnorm(alpha/2, mu_hat_M,sigma_m)
UCLM <- qnorm(1 - (alpha/2), mu_hat_M,sigma_m)

# Plotting MRCC Control Chart on Phase 2
plot(data_phase2$Temp3pm, col=ifelse(data_phase2$Temp3pm>UCLM, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCLM, 16, 1),
     main= "MRCC", ylim=c(10,45), ylab= "Temperature", xlab="Observations (phase 2)")
lines(x=1:123,y=LCLM, lty = 1,lwd = 1,col = 'red')
lines(x=1:123,y=UCLM, lty = 1,lwd = 1, col = 'red')

# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCLM) #integer(0)
which(abs(data_phase2$Temp3pm)>UCLM) #30  33  53  91 102 106

################ MM-Regression Control Chart #########################
# Fits the MM-regression model on Phase 1
mod_rob = lmrob(Temp3pm ~ Rainfall + Sunshine  + WindGustSpeed +
                  WindSpeed3pm + Humidity3pm + Pressure3pm, data = data_phase1)

# Store
coeff_rob <- as.numeric(mod_rob$coefficients)
# Selecting sigma
sigma_mm = mod_rob$scale

# Computing y hat on Phase 2
mu_hat_MM <- cbind(1,X_phase2)%*%coeff_rob

# MMRCC Control limits on Phase 2 
LCLMM <- qnorm(alpha/2, mu_hat_MM,sigma_mm)
UCLMM <- qnorm(1 - (alpha/2), mu_hat_MM,sigma_mm)

# Plotting MMRCC Control Chart on Phase 2
plot(data_phase2$Temp3pm, col=ifelse(data_phase2$Temp3pm>UCLMM, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCLMM, 16, 1),
     main= "MMRCC", ylim=c(10,45), ylab= "Temperature", xlab="Observations (phase 2)")
lines(x=1:123,y=LCLMM, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCLMM, lty = 1,lwd = 1, col = 'red')

# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCLMM) #integer(0)
which(abs(data_phase2$Temp3pm)>UCLMM) #30  33  53  87 102 106

############## WLS-Regression Control Chart ################################
# Fits the WLS regression model on Phase 1
# Computing the Weight matrix
X_phase1 <- cbind(data_phase1$Rainfall,data_phase1$Sunshine,data_phase1$WindGustSpeed,
                     data_phase1$WindSpeed3pm,data_phase1$Humidity3pm,data_phase1$Pressure3pm)
sigma2 = (summary(lm(Temp3pm ~ Rainfall + Sunshine  + WindGustSpeed +
                       WindSpeed3pm + Humidity3pm + Pressure3pm, data = data_phase1))$sigma)^2
xmod = cbind(1,X_phase1)
weights = 1/diag(sigma2*xmod%*%(solve(t(xmod)%*%xmod))%*%t(xmod))

## Weighted Least Squares
mod_lm2 = lm(Temp3pm ~ Rainfall + Sunshine  + WindGustSpeed +
               WindSpeed3pm + Humidity3pm + Pressure3pm,
               data = data_phase1, weights = weights)

# Store
coef_lm2 <- as.numeric(mod_rob$coefficients)
# Selecting sigma
sigma_wls = mad(residuals(mod_lm2))

# Computing y hat on Phase 2
mu_hat_wls <- cbind(1,X_phase2)%*%coef_lm2

# WLSRCC Control limits on Phase 2 
LCLWLS <- qnorm(alpha/2, mu_hat_wls, sigma_wls)
UCLWLS <- qnorm(1 - (alpha/2), mu_hat_wls, sigma_wls)

# Plotting WLSRCC Control Chart on Phase 2
plot(data_phase2$Temp3pm, col=ifelse(data_phase2$Temp3pm>UCLWLS, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCLWLS, 16, 1),
     main= "WLSRCC",ylim=c(10,45), ylab= "Temperature", xlab="Observations (Phase 2)")
lines(x=1:123,y=LCLWLS, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCLWLS, lty = 1,lwd = 1, col = 'red')

# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCLWLS) #integer(0)
which(abs(data_phase2$Temp3pm)>UCLWLS) #30  33  53  87  91 102 106

############## ETK-Regression Control Charts ################################
# Defining some width hyperparameters

S1 = function(y1, y2, frac = 0.5){
  n = length(y1)
  m = floor(n*frac)
  idx1 = sample(1:n, m, replace = T)
  idx2 = sample(1:n, m, replace = T)
  tmp = (y1[idx1] - y2[idx2])^2
  mean(quantile(tmp[tmp != 0], probs = c(.9, .1)))
}

S2 = function(y1, y2){
  D = outer(y1, y2, '-')
  D = D^2
  D_no_zero = D[which(!D == 0)]
  median(D_no_zero)
}

S3 = function(y1, y2, n, p){
  sum((y1-y2)^2)/(n-p-1)
}

S4 = function(y1,y2){
  h.select(y1,y2,method="aicc")
}

# Defining the Gaussian Kernel
gauss_kern = function(a, b, s){
  as.vector(exp(-( 1/s)*(a-b)^2))
}

# Defining the ETKRR model
ETKRR = function(y, x, s, tol = 1e-10, maxit = 100, tolsolve = 1e-100) {
  
  invg = 0
  x = as.matrix(x)
  n = nrow(x)
  p = ncol(x)
  
  x = cbind(1, x)
  
  # Initialization
  
  txkx = t(x)%*%x
  det.txkx = det(txkx)
  if(det.txkx == 0 || abs(det.txkx) < tol) {invtxkx = ginv(txkx); invg = invg + 1} else {invtxkx = solve(txkx, tol = tolsolve)}
  
  
  betahat = invtxkx%*%t(x)%*%y
  yhat = x%*%betahat
  
  hparameter = switch(s, 
                      S1 = S1(y, yhat),
                      S2 = S2(y, yhat),
                      S3 = S3(y, yhat, n, p),
                      S4 = S4(y, yhat))
  
  K = gauss_kern(y, yhat, hparameter)
  S = sum(2 - 2*K)
  
  it = 1
  
  # Model Step
  
  repeat {
    it = it+1
    
    txkx = t(x)%*%diag(K)%*%x
    det.txkx = det(txkx)
    if(det.txkx == 0 || abs(det.txkx) < tol) {invtxkx = ginv(txkx); invg = invg + 1} else {invtxkx = solve(txkx, tol = tolsolve)}
  
    betahat = invtxkx%*%t(x)%*%diag(K)%*%y
    yhat = x%*%betahat
    K = gauss_kern(y, yhat, hparameter)
    residuals = y - yhat
    S = c(S, sum(2-2*K))
    if (abs(S[it]-S[(it-1)]) <= tol || it >= maxit) break
  }
  (result = list(coefficients = as.vector(betahat), fitted.values = as.vector(yhat), 
                 residuals = as.vector(residuals), criterion = S, weigth = K, iter = it, nginv = invg, hp = hparameter))
}

# Fits the ETKRR models on Phase 1 accoring to the hyperparameter
model_s1 <- ETKRR(data_phase1$Temp3pm, X_phase1,s="S1")
model_s2 <- ETKRR(data_phase1$Temp3pm, X_phase1,s="S2")
model_s3 <- ETKRR(data_phase1$Temp3pm, X_phase1,s="S3")
model_s4 <- ETKRR(data_phase1$Temp3pm, X_phase1,s="S4")

# Storing the coefficients
beta_s1 <- model_s1$coefficients
beta_s2 <- model_s2$coefficients
beta_s3 <- model_s3$coefficients
beta_s4 <- model_s4$coefficients

# Selecting sigma as the mad of residuals
sd_model_etkrr1 <- mad(model_s1$residuals)
sd_model_etkrr2 <- mad(model_s2$residuals)
sd_model_etkrr3 <- mad(model_s3$residuals)
sd_model_etkrr4 <- mad(model_s4$residuals)

# Computing y hat on Phase 2
mu_hat_etkrr1 <- cbind(1,X_phase2)%*%beta_s1
mu_hat_etkrr2 <- cbind(1,X_phase2)%*%beta_s2
mu_hat_etkrr3 <- cbind(1,X_phase2)%*%beta_s3
mu_hat_etkrr4 <- cbind(1,X_phase2)%*%beta_s4

##### ETKRR Control Limits on Phase 2 #####

#ETKRR S1
LCL1 <- qnorm(alpha/2, mu_hat_etkrr1, sd_model_etkrr1)
UCL1 <- qnorm(1 - (alpha/2), mu_hat1, sd_model_etkrr1)

#ETKRR S2
LCL2 <- qnorm(alpha/2, mu_hat_etkrr2, sd_model_etkrr2)# ; LIC2
UCL2 <- qnorm(1 - (alpha/2), mu_hat2, sd_model_etkrr2)# ; LSC2

#ETKRR S3
LCL3 <- qnorm(alpha/2, mu_hat_etkrr3, sd_model_etkrr3)# ; LIC3
UCL3 <- qnorm(1 - (alpha/2), mu_hat3, sd_model_etkrr3)# ; LSC3

#ETKRR S4
LCL4 <- qnorm(alpha/2, mu_hat_etkrr4,sd_model_etkrr4)# ; LIC4
UCL4 <- qnorm(1 - (alpha/2), mu_hat4,sd_model_etkrr4)# ; LSC4

##### Plotting ETKRCC Control Charts on Phase 2 ######
######### ETKRR S1
plot(data_phase2$Temp3pm, col=ifelse(data_phase2$Temp3pm>LSC1, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>LSC1, 16, 1),
     main= "ETKRCC S1",ylim=c(10,45), ylab= "Temperature", xlab="Observations (Phase 2)")
lines(x=1:123,y=LCL1, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCL1, lty = 1,lwd = 1, col = 'red')
# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCL1) #integer(0)
which(abs(data_phase2$Temp3pm)>UCL1) #30  33  53 87 102 106
#############

######### ETKRR S2
plot(data_phase2$Temp3pm,col=ifelse(data_phase2$Temp3pm>UCL2, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCL2, 16, 1),
     main= "ETKRCC S2", ylim=c(10,45), ylab= "Temperature", xlab="Observations (Phase 2)")
lines(x=1:123,y=LCL2, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCL2, lty = 1,lwd = 1, col = 'red')
# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCL2) #integer(0)
which(abs(data_phase2$Temp3pm)>UCL2) #30  33  53  85  87  91 102 106
#############

######### ETKRR S3
plot(data_phase2$Temp3pm,col=ifelse(data_phase2$Temp3pm>UCL3, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCL3, 16, 1),
     main= "ETKRCC S3", ylim=c(10,45), ylab= "Temperature", xlab="Observations (Phase 2)")
lines(x=1:123,y=LCL3, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCL3, lty = 1,lwd = 1, col = 'red')
# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCL3) #integer(0)
which(abs(data_phase2$Temp3pm)>UCL3) #30  33  53  87  91 102 106
#############

######### ETKRR S4
plot(data_phase2$Temp3pm, col=ifelse(data_phase2$Temp3pm>UCL4, "blue", "black"), 
     pch = ifelse(data_phase2$Temp3pm>UCL4, 16, 1),
     main= "ETKRCC S4",ylim=c(10,45), ylab= "Temperature", xlab="Observations (Phase 2)")
lines(x=1:123,y=LCL4, lty = 1,lwd = 1, col = 'red')
lines(x=1:123,y=UCL4, lty = 1,lwd = 1, col = 'red')
# Checking the points out of control on Phase 2
which(abs(data_phase2$Temp3pm)<LCL4) #integer(0)
which(abs(data_phase2$Temp3pm)>UCL4) #30  33  53  85  87  91 102 106
#############


