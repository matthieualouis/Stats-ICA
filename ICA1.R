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
nox.res <-residuals(fit);
nox.stdres <- rstandard(fit);
qqnorm(nox.stdres);
qqline(nox.stdres, col = 2);



