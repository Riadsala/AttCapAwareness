data {
	int<lower=1> N; // num data points
  int<lower=1> J; // num subjects
  int<lower=0> P; // number factors
  real<lower=0> Y[N]; // to be modelled
  // fixed effects
  matrix[N,P] X; 
  // random effects
  int<lower=1, upper=J> subj[N]; //subject id
}

parameters {
  vector[P] beta; // intercept and slope
  real<lower=0> sigma_e; //error sd
  //real<lower=0>sigma_u[P]; // random subject effects 
}

model {

  beta[1] ~ normal(1,2);
  
  Y ~ lognormal(X*beta, sigma_e);
}
