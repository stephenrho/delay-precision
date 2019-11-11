// Mixture model for
// Age differences in the precision of memory at short and long delays
// Rhodes, Abbene, Meierhofer, & Naveh-Benjamin

data {
  int<lower=0> N;             // N observations
  int<lower=0> J;             // number of participants
  int<lower=1,upper=J> j[N];  // participant id
  vector[N] p_x;              // presented x coordinate
  vector[N] p_y;              // presented y coordinate
  vector[N] r_x;              // recalled x coordinate
  vector[N] r_y;              // recalled y coordinate
  real<lower=0> area;         // for determining uniform (guess) density

  int<lower=1> P;             // number of parameters (assumed same for pmem and sigma) - includes intercept term
  int<lower=1> C;             // number of conditions in the contrast matrix (for cont), usually = P
  matrix[N,P] x_m;            // design matrix for the mixture (pmem) parameter
  matrix[N,P] x_s;            // design matrix for the sigma (imprecision) parameter
  matrix[C,P] cont_m;         // matrix of contrast codes for each condition (used to calculate pmem for each condition)
  matrix[C,P] cont_s;         // as above, but for sigma
}

parameters {
  vector[P] beta_m;           // fixed effects for pmem
  vector[P] beta_s;           // fixed effects for sigma

  vector<lower=0>[P] tau_m;   // pmem scales
  vector<lower=0>[P] tau_s;   // sigma scales

  vector[P] z_m[J];           // used to specify random effects
  vector[P] z_s[J];

  cholesky_factor_corr[P] L_m;// prior on cholesky factor of correlation matrix for pmem
  cholesky_factor_corr[P] L_s;// as above but for sigma
}

transformed parameters {
  vector[P] b_m[J];           // mean + individual params
  vector[P] b_s[J];
  {
    matrix[P,P] Sigma_m;      // random effects correlation matrix
    matrix[P,P] Sigma_s;
    Sigma_m = diag_pre_multiply(tau_m, L_m);
    Sigma_s = diag_pre_multiply(tau_s, L_s);
    for (i in 1:J){
      b_m[i] = beta_m + Sigma_m * z_m[i];
      b_s[i] = beta_s + Sigma_s * z_s[i];
    }
  }
}

model {
  // priors
  tau_m ~ cauchy(0,2.5);
  tau_s ~ cauchy(0,2.5);

  L_m ~ lkj_corr_cholesky(2.0);
  L_s ~ lkj_corr_cholesky(2.0);

  beta_m ~ normal(0,5);
  beta_s ~ normal(0,5);

  for (i in 1:J){
    z_m[i] ~ normal(0,1);
    z_s[i] ~ normal(0,1);
  }

  // likelihood
  for (n in 1:N)
    target += log_mix(inv_logit(dot_product(x_m[n,], b_m[j[n]])),
                      normal_lpdf(r_x[n] | p_x[n], exp(dot_product(x_s[n,], b_s[j[n]]))) + normal_lpdf(r_y[n] | p_y[n], exp(dot_product(x_s[n,], b_s[j[n]]))),
                      log(1/area));
}


generated quantities {
  vector[N] log_lik;
  vector[C] m_out;
  vector[C] s_out;

  for (n in 1:N)
    log_lik[n] = log_mix(inv_logit(dot_product(x_m[n,], b_m[j[n]])),
                      normal_lpdf(r_x[n] | p_x[n], exp(dot_product(x_s[n,], b_s[j[n]]))) + normal_lpdf(r_y[n] | p_y[n], exp(dot_product(x_s[n,], b_s[j[n]]))),
                      log(1/area));

  m_out = cont_m*beta_m; // on logit scale
  s_out = cont_s*beta_s; // on log scale
}
