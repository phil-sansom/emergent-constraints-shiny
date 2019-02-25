data{
  int<lower=0> M;               // number of models
  vector[M] x;                  // model predictors
  vector[M] y;                  // model responses
  real z;                       // observation
  real<lower=0> sigma_z;        // observation error sd
  real mu_alpha;                // prior mean for alpha
  real<lower=0> sigma_alpha;    // prior sd for alpha
  real mu_beta;                 // prior mean for beta
  real<lower=0> sigma_beta;     // prior sd for beta
  real<lower=-1,upper=+1> rho;  // prior correlation between alpha and beta
  real mu_sigma;                // prior mean for sigma
  real<lower=0> sigma_sigma;    // prior sd for sigma
  real mu_xstar;                // prior mean for xstar
  real<lower=0> sigma_xstar;    // prior sd for xstar
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
  real xstar;                   // real world predictor
}
model{
  // Priors
  theta ~ multi_normal(mu, Sigma);
  sigma ~ normal(mu_sigma, sigma_sigma);
  xstar ~ normal(mu_xstar, sigma_xstar);

  // Likelihood
  z ~ normal(xstar, sigma_z);
  y ~ normal(X * theta, sigma);
}
generated quantities{
  real ystar;                   // predicted response

  ystar = normal_rng(theta[1] + theta[2] * xstar, sigma);
}
