# Statistical Data and Analysis - ICA. 

# Import the data:

data <- read.table('emissionssw.dat', header = T)

# Initial Matrix plot of all variables.
pairs(data)

# Plots of Pairs of variables
plot(data$noxem,data$nox)
plot(data$ws,data$nox)
plot(data$humidity,data$nox)

# Auto-correlation ! 
acf(data$nox)


### Multivariate Linear Regression ### 

X = as.matrix(data[,c('noxem','ws','humidity')]);
Y = as.matrix(data[,'nox']);

#  Linear Regression
betahat = solve(t(X) %*% X)%*%t(X)%*% Y ;

# RSS
RSS = t(Y)%*%Y - t(betahat)%*%t(X)%*%Y;

# Residuals
ehat = Y - X%*%betahat;

# Overall Standardised Residuals

fit <- lm(Y ~ X[,1] + X[,2] + X[,3]);
summary(fit);
plot(residuals(fit));


