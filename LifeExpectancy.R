data = LifeExpectancy

# Create Models
## lm(DV ~ IV)
model_pptv = lm(data$LE ~ data$PPTV)
model_pptv

model_ppp = lm(data$LE ~ data$PPP)
model_ppp

#############################################################
# Check Assumptions

# a. Construct Plot => Check Normality,	Equal Variance,	Checking for Outliers
## plot(IV, DV)
windows()
par(mfrow = c(2,2))
plot(model_pptv, main="LE vs PPTV Assumption Plots")

windows()
par(mfrow = c(2,2))
plot(model_ppp, main="LE vs PPP Assumption Plots")

par(mfrow = c(1,1))
hist(model_pptv$residuals)

par(mfrow = c(1,1))
hist(model_ppp$residuals

# b. Construct Scatterplots => Check Linearity
windows()
plot(data$PPTV, data$LE, main="Scatter Plot for Life Expectancy vs. People per TV", xlab="People per TV", ylab="Life Expectancy")
abline(model_pptv, col="blue", lwd=2)

windows()
plot(data$PPP, data$LE, main="Scatter Plot for Life Expectancy vs. People per Physician", xlab="People per Physician", ylab="Life Expectancy")
abline(model_ppp, col="red", lwd=2)

#############################################################

# LE vs PPTV model
summary(model_pptv)

# LE vs PPP model
summary(model_ppp)

# c. Regression Line Equations
## LEPPTV^ = 69.6481 - 0.0363(PPTV)
## LEPPP^ = 69.9264 - 0.0007374(PPP)
    
# e. Significance Test for an Alpha Level of 5%
# f. Coefficient of Determination (r2)
#############################################################

# d. Correlation Coefficient (r)

cor(data$PPTV, data$LE)
cor(data$PPP, data$LE)

#############################################################

# g. Predict LEs for New Countries (Ghana & Bhutan) on PPTV and PPP Data on Web Search

range(data$PPTV)   # 1.3 592.0
range(data$PPP)    # 226 36660

69.6481 - (0.0363*79.93287)
69.9264 - (0.0007374*1000)

# Ghanna
# People per TV (PPTV) (by 2029)
34920000 / 223600  # 156.1717 people (est.)
# LE^
69.6481 - (0.0363*156.1717)  # 63.97907 ~ 64 years

# Bhutan
# People per Physician (PPP) (in 2022): 1,000
# LE^
69.9264 - 0.0007374(1000)  # 69.189 ~ 69.2 years
#############################################################

# h. Predict PPTV and PPP for Djibouti

model_pptv_inv = lm(PPTV ~ LE, data=data)
model_pptv_inv
# predict(model_pptv_inv, newdata=data.frame(LE=66.41))   # 65.67502
737.85 - (10.12*66.41)  # 65.7808 ~ 65 people per TV

model_ppp_inv = lm(PPP ~ LE, data=data)
model_ppp_inv
# predict(model_ppp_inv, newdata=data.frame(LE=66.41))  # 3567.156
34651.4 - (468.1*66.41) # 3564.879 ~ •	3564 people per physician

#############################################################
