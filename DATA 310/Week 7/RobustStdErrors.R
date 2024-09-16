library(sandwich)

# Sets the seed
set.seed(12221979)
#
# Parameters for simulation
#
numsims <- 1000
numobs <- 500
alpha <- 1
beta <- 2
lb <- .025
ub <- .975

# Initializes a matrix with numobs rows and two columns
data <- matrix(-9, nrow = numobs, ncol = 3)
# Sets the value of the modeled variable 
#[,1] <- runif(numobs, min = 0, max = 1) 
#data[,1] <- runif(numobs, min = 0, max = 0.05) 
data[,1] <- runif(numobs, min = 0.5, max = 0.55)

numwithinCIols <- 0
numwithinCIrobust <- 0 

for (s in 1:numsims) {
  
  #Construct the value of the error term
  data[,2] <- rnorm(numobs, mean=0, sd = data[,1])
  
  # Construct the value of the outcome variable
  data[,3] <- alpha + beta*data[,1] + data[,2]

  data.df <- as.data.frame(data)
  plot(data.df$V1, data.df$V3)
  temp <- lm(V3 ~ V1, data = data.df)

  
  # Extracts the slope coefficient on the modeled variables
  betahat <- temp$coefficients[2]

  # Extracts the ols standard error on the slope coefficient on the modeled variables
  tempvar <- vcov(temp)
  beta_se_ols <- tempvar[2, 2]^(1/2) 

  # Constructs the lower bound on the CI on beta using the OLS standard error
  lower_bound_ols <- betahat + beta_se_ols*qt(lb, numobs - 2)
  # Constructs the upper bound on the CI on beta using the OLS standard error
  upper_bound_ols <- betahat + beta_se_ols*qt(ub, numobs - 2)
  
  # Is the true beta within the confidence interval construted using OLS standard errors
  withinCI <- ((lower_bound_ols < beta) & (upper_bound_ols > beta)) 
  
  # Keeps running total of whether true beta is within the ols confidence interval
  numwithinCIols <- numwithinCIols + withinCI
  
  # Extracts the White standard error on the slope coefficient on the modeled variables
  tempvar <- vcovHC(temp, type="HC1")
  beta_se_robust <- tempvar[2, 2]^(1/2) 

  # Constructs the lower bound on the CI on beta using the OLS standard error
  lower_bound_robust <- betahat + beta_se_robust*qt(lb, numobs - 2)
  # Constructs the upper bound on the CI on beta using the OLS standard error
  upper_bound_robust <- betahat + beta_se_robust*qt(ub, numobs - 2)

  # Is the true beta within the confidence interval construted using robust standard errors
  withinCI <- ((lower_bound_robust < beta) & (upper_bound_robust > beta))
  
  # Keeps running total of whether true beta is within the robust confidence interval
  numwithinCIrobust <- numwithinCIrobust + withinCI
}

print(numwithinCIols / numsims)
print(numwithinCIrobust / numsims)
