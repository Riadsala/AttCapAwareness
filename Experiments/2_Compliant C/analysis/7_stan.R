library(ggplot2)
library(rstan)
library(coda)
library(dplyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("responseCapture.csv")



fDat$observer = as.factor(fDat$observer)
rDat$observer = as.factor(rDat$observer)

# remove incorrect trials, and trials with NA pathlength
rDat$okTrial = 
	rDat$targDiscrim==1 &
	is.finite(rDat$pathLength) &
	rDat$RT > 0.150 & rDat$RT < 3


dat = filter(rDat, okTrial==1,captured=="fixated distracter")
dat= droplevels(dat)

# X = model.matrix(~congC*thought, dat)
X = model.matrix(~congC*thought, dat)



myList = list(
	N=nrow(dat),
	P = 4, 
	Y=dat$RT, 
	X= X,
	J = length(unique(dat$observer)), 
	subj = as.numeric(dat$observer))

fit <- stan(file="modelSpec.stan", data=myList, iter = 1000, chains = 4)

print(fit, pars = c("beta"),probs = c(0.025, 0.5, 0.975))


post_beta<-As.mcmc.list(fit,pars="beta")
plot(post_beta)
