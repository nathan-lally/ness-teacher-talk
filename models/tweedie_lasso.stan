data {
  int<lower=1> N; // sample size
  int<lower=1> P; // columns in the design matrix
  int<lower=1> M; // controls precision of compound Poisson-gamma approximation
  real<lower=0> Y[N]; // response variable
  vector[N] lnexposure; // offset
  matrix[N,P] X; // design matrix
}
parameters {
  real omega_0; // intercept
  vector[P] omega; // regression weights
  real<lower=0> tau; // shrinkage parameter for Bayes lasso prior on regression weights
  real<lower=0> phi; // dispersion parameter of Tweedie
  real<lower=1, upper=2> theta; // power parameter of tweedie
}
transformed parameters {
  real<lower=0> alpha; // gamma shape
  alpha = (2-theta)/(theta-1);
}
model {
  // some calculations
  vector[N] mu;
  vector[N] lambda;
  vector[N] beta;
  for(n in 1:N){
    // tweedie mean
    mu[n] = exp(omega_0 + X[n] * omega + lnexposure[n]);
    // poisson rate
    lambda[n] = 1/phi*mu[n]^(2-theta)/(2-theta);
    // gamma rate
    beta[n] = 1/phi*mu[n]^(1-theta)/(theta-1);
  }
  // priors
  phi ~ student_t(4, 0, 1);
  theta ~ uniform(1, 2);
  omega_0 ~ normal(0, 10);
  omega ~ double_exponential(0, tau);
  tau ~ student_t(4, 0, 1);
  // likelihood
  for (n in 1:N) {
    if (Y[n] == 0) {
      target += -lambda[n];
    } else {
      vector[M] ps;
      for (m in 1:M)
        ps[m] = poisson_lpmf(m | lambda[n]) + gamma_lpdf(Y[n] | m*alpha, beta[n]);
      target += log_sum_exp(ps);
    }
  }
}
