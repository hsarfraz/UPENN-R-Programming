# Set directory for class material here
setwd("~/Box Sync/Teaching/Data201/")

# Packages to install (uncomment if needed)
#install.packages("survey")
#install.packages("readxl")

#
# New York 22nd House Poll Example
#

# Displays estimate of population support
ny22 <- read.csv("RawData/elections-poll-ny22-3.csv")
library(survey)
ny22w <- svydesign(ids = ~1, 
                   data = ny22, weights = ny22$final_weight)
ny22output <- svymean(~response, ny22w)
print(ny22output)

# Displays a symmetric 95 percent CI on the population parameters
numobs <- nrow(ny22)
confint(ny22output, level = 0.95, df = numobs - 1)

# Showing how exactly the 95 percent CI get calculated
print(coef(ny22output))
print(vcov(ny22output))
print(coef(ny22output)[2] + 
        vcov(ny22output)[2, 2]^(1/2)*qt(0.025, numobs - 1))
print(coef(ny22output)[2] + 
        vcov(ny22output)[2, 2]^(1/2)*qt(0.975, numobs - 1))

# Displays estimate of male support
ny22men <- svymean(~response, subset(ny22w, gender == "Male"))
print(ny22men)

# Displays estimate of female support
ny22female <- svymean(~response, subset(ny22w, gender == "Female"))
print(ny22female)

# Displays 95 percent CI on difference in male and female support
tt <- svyttest(I(response == "Rep")~gender, ny22w)
print(tt)
confint(tt, level = 0.95, df = numobs - 2)

# How to use regression to estimate and put CI on population support
meanreg <- lm(I(response == "Rep")~1, weight = final_weight, data = ny22)
summary(meanreg)
confint(meanreg, level = 0.95, df = meanreg$df.residual)

# How to use regression to estimate and put CI on difference in group support
diffreg <- lm(I(response == "Rep")~gender, weight = final_weight, data = ny22)
summary(diffreg)
confint(diffreg, level = 0.95, df = meanreg$df.residual)

#
# Comparing Turnout in Treatment and Control Group
#

# How to calculate p-value on difference-in-means by "hand"
SA <- 1/399*(140*(1 -.35)^2 + 260*(0 -.35)^2)
SB <- 1/499*(190*(1 -.38)^2 + 310*(0 -.38)^2)
SE <- (SA/400 + SB/500)^(1/2)
nu <- (SA/400 + SB/500)^2 / 
  ((SA/400)^2/399 + (SA/500)^2/499)
T <- (-(0.35 - 0.38) / SE)
pt(T, nu)

library(readxl)
# How to use regression to calculate the p-value om the difference-in-means
LectureDifferenceMeans <- read_excel("RawData/LectureDifferenceMeans.xlsx")
reg1 <- lm(Voted ~ Mobilized, data = LectureDifferenceMeans)
reg1summary <- summary(reg1)
print(reg1summary)
# Symmetric 95 percent CI on difference in means
confint(reg1, level = 0.95)

# How to construct a 95 percent CI on the constant
print(reg1summary$coefficients)
print(reg1summary$df)
print(reg1summary$coefficients[1, 1]  + 
        reg1summary$coefficients[1, 2]*
        qt(0.025, df = reg1summary$df[2]))
print(reg1summary$coefficients[1, 1]  + 
        reg1summary$coefficients[1, 2]*
        qt(0.975, df = reg1summary$df[2]))

# How to construct a 95 percent CI on the constant + slope
vcov = vcov(reg1)
print(vcov)
print(vcov^(1/2))
alphabeta_se <- (vcov[1, 1] + 2*vcov[2, 1] +
                   vcov[2, 2])^(1/2)
print(reg1summary$coefficients[1, 1]  + 
        reg1summary$coefficients[2, 1] +  
        alphabeta_se*
        qt(0.025, df = reg1summary$df[2]))
print(reg1summary$coefficients[1, 1]  + 
        reg1summary$coefficients[2, 1] +
        alphabeta_se* 
        qt(0.975, df = reg1summary$df[2]))