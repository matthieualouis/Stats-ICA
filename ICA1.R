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

nox.fit <- lm(Y ~ X[,1] + X[,2] + X[,3]);
summary(nox.fit);
nox.res <-residuals(nox.fit);
nox.stdres <- rstandard(nox.fit);

# Some analytics plots
par(mfrow=c(2,2))
plot(nox.fit,which=c(1,2,3,4),ask=FALSE)

# Plotting QQ plot of standardised Residuals
qqnorm(nox.stdres);
qqline(nox.stdres, col = 2);

# Plotting Residuals with respect to Predictors:
par( oma=c(1,1,3,1) );
par( mfrow=c(1,3) );
plot( nox.stdres, X[,1], ylab="Noxem predictors", xlab="Standardised residuals" )
plot( nox.stdres, X[,2], ylab="Wind Speed predictors", xlab = "Standardised residuals")
plot( nox.stdres, X[,3], ylab="Humidity predictors", xlab = "Standardised residuals")


