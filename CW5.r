# Name: Xuan Jin
# Student ID: s2477282 

## Overview: 
# This code wants to use JAGS to implement monotonic regression which means one 
# variable depends monotonically on another. In this case, we should investigate 
# the monotonic relation between the concentration of vinclozolin in nano-Moles
# and the measured AR activity(effect). 
#
# There are mainly 3 parts:
# Firstly, code up the model 1 in "M1s2477282.jags" file and also simulate it to 
# get sampled parameters. Try a series of attempts to adjust and improve model 1
# base on the trace plots with highest and lowest effective sample size. The final
# plots show this model is converged. Then, I produce a plot for given effect data
# using symbols and mean expected effect along with a 95% Credible Regions using lines.
#
# Secondly, set up another similar but different model 2 in "M2s2477282.jags". 
# This time we suppose model has own decreasing parameter mu in each experimental run.
# The modified model 2 is more targeted and specific for concentration level. Similarly,
# produce a effect plot like the one for model 1.
#
# Finally, compute the DIC values for model 1 and model 2 and select preferable one.
# The result DIC values show the second model is better.


# write a function for plotting expected effect in the future parts.
plot.effect <- function(data, samples, model.type, Ntotal, Nrun){
  # This function aims to produce a plot that can show given effect data against
  # the dose index from 1 to 9 (symbols) and overlayed the mean expected effect 
  # (lines) according to different models along with 95% credible regions. The 
  # lines and symbols color coded by experimental run. 
  #
  # input:
  #  - data: a matrix contains experimental data from reading given file
  #  - samples: a mcmc list output of parameters for each chain after simulation
  #  - model.type: a string variable to distinguish model 1 and model 2
  #  - Ntotal: an integer of the total number of doex index
  #  - Nrun: an integer of the number of experimental runs
  #
  # output:
  #  - a desired mean effected plot along with 95% credible regions under samples
  
  # prepare plotting data by creating 3 new necessary columns 
  plotData <- transform(data, MeanEffect = NA, LowerHPDI = NA, UpperHPDI = NA, DoexIdx = NA)
  
  # combine all chains' result together to compute mean expected effect
  combinedSamples <- do.call(rbind, lapply(samples, as.matrix))

  # use for loops to calculate mean expected effect and its 95% CI
  for(j in 1:Nrun) {
    for(i in 1:Ntotal) {
      ## find the matrix position we need to fill
      idx <- (j-1) * Ntotal + i 
      ## extract jth M vector from samples
      M_j <- combinedSamples[,paste0("M[", j, "]")]
      ## calculate the expected effect vector according to different models
      if (model.type == "Model 1"){
        mu_i <- combinedSamples[,paste0("mu[", i, "]")]
        list.eff <- M_j * mu_i}
      else{
        mu_ij <- combinedSamples[,paste0("mu[", i, ",", j, "]")]
        list.eff <- M_j * mu_ij}
      ## calculated the 95% CI for expected effect list
      CI <- HPDinterval(as.mcmc(list.eff))
      ## calculated mean expected effect and fill these values into our matrix
      plotData$MeanEffect[idx] <- mean(list.eff)
      plotData$LowerHPDI[idx] <- CI[1]
      plotData$UpperHPDI[idx] <- CI[2]
      plotData$DoesIdx[idx] <- i       # specify x-axis for our plot below
    }
  }
  
  # use filled 'plotData' to obtain our desired plot
  p <- ggplot(plotData, aes(x = DoesIdx, y = effect, color = factor(exper))) +
    geom_point() +                                              # plot the actual given data points
    geom_line(aes(y = MeanEffect, linetype = "Mean Effect")) +  # add lines for mean effects
    geom_ribbon(aes(ymin = LowerHPDI, ymax = UpperHPDI, fill = factor(exper)), 
                alpha = 0.2, linetype = 2) +           # add shaded area for HPDI
    scale_color_brewer(type = "qual") +                # use a better color palette
    scale_fill_brewer(type = "qual", guide = "none") +  # use a color palette for HPDI, no legend for fill
    scale_x_continuous(breaks = 1:9) +          # set x axis breaks to show each integer from 1 to 9
    labs(title = paste("Effect vs Dose Index for",model.type),  # set suitable x,y labels and titles
         subtitle = "Original Data and Predicted Mean Expected Effect with 95% Credible Regions",
         x = "Dose Index",
         y = "Effect",
         color = "Experimental Run with 95% CI") +
    theme_minimal()               # use a minimal theme for better aesthetics
  
  print(p) # explicitly print the plot object
}


# load the packages that the code needs
library('rjags')
library('coda')
library('ggplot2')

# read the data from given table "vin.txt"
data <- read.table("vin.txt", header = TRUE)

Ntotal <- length(unique(data$conc))       ## get the number of concentration levels(Does Index)
Nrun <- length(unique(data$exper))        ## get the number of runs

# assign two jags file to two variables that will be used later 
model1 <- "M1s2477282.jags"  
model2 <- "M2s2477282.jags"

# define a list that contains stochastic parameters and should be sampled and checked
params <- c('M','mu','tau','tau0')
# construct column 'effect' in data into a matrix form
effect <- matrix(data$effect ,nrow = Ntotal ,ncol = Nrun)
# define a list that contains the initial data that JAGS models need
input <- list(effect = effect, Nrun = Nrun, Ntotal = Ntotal)

# read data into JAGS and the coded model1 to compile using 4 chains
# use adaptive iteration as a pre-burn-in phase to improve efficiency
jags.mod1 <- jags.model(model1, 
                        data = input,
                        n.chains = 4,
                        n.adapt = 500)
# take compiled jags.mod1 to simulate from the model posterior using 12000 iterations
# with 1500 burn-in values for adjustments based on trace plots, which helps to reach 
# a state closer to equilibrium and to improve mixing and convergences.
samples1 <- coda.samples(jags.mod1, 
                        variable.names = params, 
                        n.iter = 12000,
                        n.burnin = 1500)

# get the effective sample size of each parameters from the posterior
effSizes <- effectiveSize(samples1) 
# get parameters with the highest and lowest effective sample size and their values respectively
# we need to exclude parameters with 0 effective sample size here
maxEffSizeParam <- names(which.max(effSizes))
minEffSizeParam <- names(which.min(effSizes[effSizes > 0]))
cat(paste('The parameter with the highest effective sample size:', maxEffSizeParam, '\n',
          'The highest effective sample size:', effSizes[maxEffSizeParam], '\n',
          'The parameter with the lowest effective sample size:', minEffSizeParam, '\n',
          'The lowest effective sample size:', effSizes[minEffSizeParam]))

# Check the model convergence:
# plot 2 trace plots for the parameter with the highest and lowest effective sample size together
## pdf("trace_plots.pdf", height=10, width=12)
## par(mfrow=c(2,1))
traceplot(samples1[,maxEffSizeParam] , main=paste("Trace Plot for", maxEffSizeParam,"for Model 1"), 
          xlab="Iteration", ylab="Value")
traceplot(samples1[,minEffSizeParam], main=paste("Trace Plot for", minEffSizeParam,"for Model 1"), 
          xlab="Iteration", ylab="Value")
## dev.off()
# diagnostic trace plots show this model has converged as 4 chains trends are almost stable
# and have rapid movement and systematic drifts after some attempts and made adjustments, 
# after that the mixing effect could be more ideal so can predict well for target distribution.

# similarly, the acfplot shows the autocorrelation is decreased a lot for the parameter
# with the lowest effective sample size.
# acfplot(samples1[,minEffSizeParam], main=paste("ACF Plot for", minEffSizeParam), 
#           xlab="Iteration", ylab="Value")

# plot mean expected effect based on the requirements for model 1
plot.effect(data, samples = samples1, model.type = "Model 1", Ntotal, Nrun)


# compile JAGS model2 using 4 chains again and adaptive iteration 800 as a pre-burn-in phase
jags.mod2 <- jags.model(model2, 
                        data = input,
                        n.chains = 4,
                        n.adapt = 500)
# take compiled jags.mod2 to simulate another different model 2 using again 12000 iterations 
# for comparison as well as a burn-in stage for the improvement of mixing and reduce auto-correlations
samples2 <- coda.samples(jags.mod2, 
                        variable.names = params, 
                        n.iter = 12000,
                        n.burnin = 1500)

# plot mean expected effect based on the requirements for model 2 like before
plot.effect(data, samples = samples2, model.type = "Model 2", Ntotal, Nrun)


# DIC(Deviance Information Criterion) value for each model and compare them 
dic1 <- dic.samples(jags.mod1,n.iter = 10000);dic1
dic2 <- dic.samples(jags.mod2,n.iter = 10000);dic2
## Model 1 ##
# |**************************************************| 100%
# Mean deviance:  581.9 
# penalty 8.714 
# Penalized deviance: 590.6 

## Model 2 ##
# |**************************************************| 100%
# Mean deviance:  559.4 
# penalty 16.29 
# Penalized deviance: 575.7 

# In conclusion, model 2 is preferable. We can see there is a decrease in DIC
# from model1 to model2 by increasing the penalty to prevent overfitting, which 
# means model2 is better as it suggests a better balance between fit and complexity.

# Moreover, from the expected effect plots, for model 1, some of the original 
# effect data points are not in the predicted 95% CI; however, for model 2, 
# the plot shows that almost all points are closely included in the credible region. 
# This also can prove that model 2 is better than model 1.
# In the future, if wants to improve the result, we can gather data over a wider 
# range of vinclozolin concentrations and under varied experimental conditions.
# And we can try more sophisticated statistical methods and tuning distributions. 









