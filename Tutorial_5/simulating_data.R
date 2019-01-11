# simulating data 
library(lattice)
library(car)
library(sn)

warren9v <- readxl::read_excel(path = "Tutorial_6/warren9v.xls")

# N observations 
nobs <- 428
# Converting all covariances to positives and extracting c/cv matrix 
warrenCV <- abs(as.matrix(warren9v[2:10, 3:ncol(warren9v)]))
# Filling in blanks 
warrenCV[is.na(warrenCV)] <- 0 
# Extracting variances for filling back in
variances <- diag(warrenCV)
# filling in top half
warrenCV <- warrenCV + t(warrenCV)
# reinserting variances 
diag(warrenCV) <- variances
# Transforming to a correlation matrix 
warrenCor <- cov2cor(warrenCV)
# Storing number of variables
nvars <- nrow(warrenCor)
# Cholesky decomposition
L <- chol(warrenCor)
# Generating random variables that follow the V/CV matrix 
simDat <- t(L) %*%  matrix(rnorm(nvars*nobs), nrow=nvars, ncol=nobs)
  # Simulating some skewed variables to make things slightly more interesting 
  # simDat2 <- t(L) %*%  matrix(rsn(nvars*nobs, xi = 0, alpha = 100, tau = 1), nrow=nvars, ncol=nobs)
# Converting to data frames
simDat <- data.frame(t(simDat))
 # simDat2 <- data.frame(t(simDat2))
 # # inserting skewed data
 # simDat$X1performance <- simDat2$X1performance
 # simDat$X2performance <- simDat2$X2performance  
 # # Simulating some more skewed variables
 # simDat2 <- data.frame(t(t(L) %*%  matrix(rsn(nvars*nobs, xi = 0, alpha = -10, tau = 1), nrow=nvars, ncol=nobs)))
 # # inserting skewed data
 # simDat$X1knowledge <- simDat2$X1knowledge
 # simDat$X2knowledge <- simDat2$X2knowledge  

splom(simDat)
cor(simDat)

#simDat$X3performance <- rowMeans( data.frame(simDat$X2performance +simDat$X1performance) ) +
#  rnorm(nrow(simDat), mean = 0, sd = mean(sd(simDat$X2performance) + sd(simDat$X1performance)))

readr::write_csv(simDat, "Tutorial_6/warrenData.csv")

