data{
  int<lower=0> M;             // number of models
  vector[M] x;                // model predictors 
  vector[M] y;                // model responses
  real z;                     // observation
  real<lower=0> sigma_z;      // observation error sd
  real mu_alpha;              // prior mean for alpha
  real<lower=0> sigma_alpha;  // prior sd for alpha
  real mu_beta;               // prior mean for beta
  real<lower=0> sigma_beta;   // prior sd for beta
  real mu_sigma;              // prior mean for sigma
  real<lower=0> sigma_sigma;  // prior sd for sigma
  real mu_xstar;              // prior mean for xstar
  real<lower=0> sigma_xstar;  // prior sd for xstar
}
parameters{
  real alpha;                 // model response intercept/mean
  real beta;                  // model response gradient
  real<lower=0> sigma;        // model response spread
  real xstar;                 // real world predictor
}
model{
  // Priors
  alpha ~ normal(mu_alpha, sigma_alpha);
  beta  ~ normal(mu_beta , sigma_beta );
  sigma ~ normal(mu_sigma, sigma_sigma);
  xstar ~ normal(mu_xstar, sigma_xstar); 
  
  // Likelihood
  z ~ normal(xstar, sigma_z);
  y ~ normal(alpha + beta * x, sigma);
}
generated quantities{
  real ystar;                 // predicted response
  
  ystar = normal_rng(alpha + beta * xstar, sigma);
}
