data{
  int<lower=0> M;         // number of models
  vector[M] x;            // model predictors 
  vector[M] y;            // model responses
  real z;                 // observation
  real<lower=0> sigmaz;   // observation error sd
  real meanalpha;         // prior mean for alpha
  real<lower=0> sdalpha;  // prior sd for alpha
  real meanbeta;          // prior mean for beta
  real<lower=0> sdbeta;   // prior sd for beta
  real meansigma;         // prior mean for sigma
  real<lower=0> sdsigma;  // prior sd for sigma
  real meanxstar;         // prior mean for xstar
  real<lower=0> sdxstar;  // prior sd for xstar
}
parameters{
  real alpha;             // model response intercept/mean
  real beta;              // model response gradient
  real<lower=0> sigma;    // model response spread
  real xstar;             // real world x
}
model{
  // Priors
  alpha ~ normal(meanalpha, sdalpha);
  beta  ~ normal(meanbeta , sdbeta );
  sigma ~ normal(meansigma, sdsigma);
  xstar ~ normal(meanxstar, sdxstar); 
  
  // Likelihood
  z ~ normal(xstar, sigmaz);
  y ~ normal(alpha + beta * x, sigma);
}
