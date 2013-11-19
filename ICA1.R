# Statistical Data and Analysis - ICA. 

# Plot the standard lin model plots
lmsum <- function(lm) {
  # Some analytics plots
  par(mfrow=c(2,2))
  plot(lm, which=c(1,2,3,4), ask=FALSE)
  par(mfrow=c(1,1))
}

# Plots important of Pairs of variables
summaryplots <- function(data) {
  # Initial Matrix plot of all variables.
  pairs(data)
  
  plot(data$noxem,data$nox)
  plot(data$ws,data$nox)
  plot(data$humidity,data$nox)
}

summarisemodel <- function(lm) {
  # Get the residuals from the data
  summary(lm)
  
  nox.res <-residuals(nox.fit);
  nox.stdres <- rstandard(nox.fit);
  
  # Plotting QQ plot of standardised Residuals
  qqnorm(nox.stdres);
  qqline(nox.stdres, col = 2);
  
  lmsum(lm)
}

# Import the data:
data <- read.table('emissionssw.dat', header = T)
summaryplots(data)

### Multivariate Linear Regression ### 
X = as.matrix(data[,c('noxem','ws','humidity')]);
Y = as.matrix(data[,'nox']);

# Simple linear case
nox.fit <- lm(nox ~ noxem^0.2 + ws^0.2 + humidity^0.2, data=data);
summarisemodel(nox.fit)
plot(rstandard(nox.fit), xlab = "Standard Residuals", ylab = "Index")



# Try some log models
logdata = log(data)

# Simple log
log.fit <- lm(nox ~ noxem + ws + humidity, data=logdata);
summarisemodel(log.fit)

# Better but OTT
loginteractions.fit <- lm(nox ~ noxem * ws * humidity, data=logdata);
summarisemodel(loginteractions.fit)

# Plotting Residuals with respect to Predictors:
par( oma=c(1,1,3,1) );
par( mfrow=c(1,3) );
plot( nox.stdres, X[,1], ylab="Noxem predictors", xlab="Standardised residuals" )
plot( nox.stdres, X[,2], ylab="Wind Speed predictors", xlab = "Standardised residuals")
plot( nox.stdres, X[,3], ylab="Humidity predictors", xlab = "Standardised residuals")
par(mfrow=c(1,1))

# Auto-correlation ! 
acf(data$nox)

