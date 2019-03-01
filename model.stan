data{
  int<lower=0> M;               // number of models
  vector[M] x;                  // model predictors
  vector[M] y;                  // model responses
  real mu_alpha;                // prior mean for alpha
  real<lower=0> sigma_alpha;    // prior sd for alpha
  real mu_beta;                 // prior mean for beta
  real<lower=0> sigma_beta;     // prior sd for beta
  real<lower=-1,upper=+1> rho;  // prior correlation between alpha and beta
  real mu_sigma;                // prior mean for sigma
  real<lower=0> sigma_sigma;    // prior sd for sigma
}
transformed data{
  vector[2]   mu;
  matrix[2,2] Sigma;
  matrix[M,2] X;

  mu[1]      = mu_alpha;
  mu[2]      = mu_beta;
  Sigma[1,1] = sigma_alpha^2;
  Sigma[2,1] = rho*sigma_alpha*sigma_beta;
  Sigma[1,2] = rho*sigma_alpha*sigma_beta;
  Sigma[2,2] = sigma_beta^2;
  X[,1]      = rep_vector(1, M);
  X[,2]      = x;
}
parameters{
  vector[2] theta;              // regression coefficients
  real<lower=0> sigma;          // model response spread
}
model{
  // Priors
  theta ~ multi_normal(mu, Sigma);
  sigma ~ normal(mu_sigma, sigma_sigma);

  // Likelihood
  y ~ normal(X * theta, sigma);
}
