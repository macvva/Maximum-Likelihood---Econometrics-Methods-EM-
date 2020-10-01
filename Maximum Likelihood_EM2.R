rm(list=ls())     # Clean memory
graphics.off()    # Close graphs
cat("\014")       # Clear Console



install.packages("maxLik")      # Package to optimize the objective function
install.packages("plotly")      # Package to make 3D plots
library(maxLik)
library(plotly)

######################################################################################
################ Weibull distribution (Greene pag. 640 exercise n. 4) ################
######################################################################################
#part a
{
n <- 100000    # n is the sample size
alpha <- 1     # Scale parameter
beta <- 3      # Shape parameter

x <- rweibull(n, shape = beta, scale = alpha)      # Generate x, which has a Weibull distribution
}

################ Objective function ################
#part b
ML.Weibull <- function(par,x){      # Objective function called ML; "par" is one of the arguments of the function and it represents the vector of the unknown parameters that have to be estimated
  alpha <- par[1]
  beta <- par[2]
  loglike <- rep(NA,n)      # Vector to store the observation-specific log-likelihood values
  for (i in 1:n){
    loglike[i] <- log(alpha*beta*x[i]^(beta-1)*exp(-alpha*x[i]^beta))      # observation-sepecific expression for the log-likelihood of a Weibull distribution
  }
  return(loglike)      # a vector of observation-specific log-likelihood values must be returned for the optimization function
}


################ Optimization ################
#part c
init.val <- c(1.5,3.5)      # Initial guess for the two parameters


## Let's optimize using the Newthon-Raphson algorithm ##
{
start.time.NR <- proc.time()      # Start the clock
optNR <- maxNR(ML.Weibull,start=init.val,iterlim=1000, x=x)      # Maximize the objective function with the Newthon-Raphson method
el.time.NR <- proc.time() - start.time.NR      # Stop the clock
print(el.time.NR)

print(optNR$maximum)      # value of the maximum loglikelihood
print(optNR$estimate)      # values of the estimated parameters
}
## Let's optimize using the BHHH algorithm ##
{
start.time.BHHH <- proc.time()      # Start the clock
optBHHH <- maxBHHH(ML.Weibull,start=init.val,iterlim=1000, x=x)      # Maximize the objective function with the BHHH method
el.time.BHHH <- proc.time() - start.time.BHHH      # Stop the clock
print(el.time.BHHH)      # "elapsed" corresponds to the computation time      

print(optBHHH$maximum)      # value of the maximum loglikelihood
print(optBHHH$estimate)      # values of the estimated parameters
}

################ Plot the loglikelihood ################

val.alpha <- c(0.1,0.5,1,1.5,2)      # Grid values for alpha (they must contain the true value of alpha)
#Note here that if we add the estimated maximum found by the algorithms above to the grid value, the plot will also maximize at this value 
val.beta <- c(1,2,3,4,5)       # Grid values for beta (they must contain the true value of beta)

loglike.val <- matrix(NA,length(val.alpha),length(val.beta))      # Matrix to store the values of the log-likelihood for the grid values of alpha and beta

for (l in 1:length(val.alpha)){      # Loop for grid values of alpha
  for (j in 1:length(val.beta)){      # Loop for grid values of beta
    alpha <- val.alpha[l]
    beta <- val.beta[j]
    loglike <- rep(NA,n)      # Vector to store the observation-specific log-likelihood values
    for (i in 1:n){
      loglike[i] <- log(alpha*beta*x[i]^(beta-1)*exp(-alpha*x[i]^beta))      # observation-sepecific expression for the log-likelihood of a Weibull distribution
    }
    loglike.val[l,j] <- sum(loglike)      # log-likelihood for the lth value of alpha and the jth value of beta
  }
}

plot_ly(x = ~val.beta, y = ~val.alpha, showscale = FALSE) %>% add_surface(z = ~loglike.val)     # 3D plot of the log-likelihood

val.alpha[which(loglike.val == max(loglike.val), arr.ind = TRUE)[1]]      # value of alpha which maximizes the log-likelihood
val.beta[which(loglike.val == max(loglike.val), arr.ind = TRUE)[2]]      # value of beta which maximizes the log-likelihood
