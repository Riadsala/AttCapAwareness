require(brms)

# dat = read(file="attcap.Rda")


dat = rDat
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(theme_bw())

hyp_estimate <- c("(exp(Intercept +  congCincongruent) - exp(Intercept)) = 0")
hyp_effect  <- c("b_congCincongruent / (sigma + sd_observer__Intercept) = 0")
# you can also add the variance component for slope, which will be a more conservative effect size

hyp_effect_conserv  <- c("b_congCincongruent / (sigma + sd_observer__Intercept +
                         sd_observer__congCincongruent) = 0")
m <- brm(RT ~ congC + (congC|observer), family= lognormal(link = "identity"), data = dat)
m <- brm(RT ~ congC + (congC|observer), dat)
hypothesis(m, hyp_estimate) # estimate 
tapply(dat$RT, dat$congC, mean) # check to ensure in same direction
hypothesis(m, hyp_effect, class = NULL ) # effect size (citation if needed)
hypothesis(m, hyp_effect_conserv, class = NULL ) # conservative effect size


# informed priors
sd(dat$RT) # 2 SD within intercept sounds reasonable

prior <- c(set_prior("normal(0, 225)", class = "b"), set_prior("cauchy(0, 150)", class = "sd"))
m_prior <- brm(RT ~ congC + (congC|observer), prior = prior, sample_prior = TRUE, data = dat)

# see what typical standard deviation is for this measure and use that for cauchy prior
# prior of "b" is saying there is 95 % prior the estimate will be within two SD's of intercept.
# In other words, we are using the empirical rule which makes sense. You can then use 112 for
# a sensitivity analysis

bayesFactor <- hypothesis(m_prior, "congCincongruent = 0")
bf <- print(bayesFactor, digits = 5)
1 / bf$hypothesis$Evid.Ratio # in favor or alterative 
plot(bayesFactor) # visualize 
