

data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  int<lower=1> nst;
  int<lower=1> nfirstobs;
  int<lower=1> nstops;
  int<lower=1> ndata;

  int<lower=1> spacetime[ncounts];
  int<lower=0> richness[ncounts];   // Species richness as a count/integer
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int<lower=1> reg[ncounts];  // Regions
  int<lower=1> stops[ncounts];

  int<lower=1> obs[ncounts];   // observers
  int<lower=1> firstobs[ncounts];
  real pforest[ncounts];  // Percent forest cover
  real xseq[ndata];
  

}



parameters {
// MAIN MODEL
  matrix[nreg, nst] a;

  vector[nreg] b_space;
  vector[nreg] b_time;
  
  vector[nobs] observer;
  real<lower=0> sdobs;
  real<lower=0> sdfirst;
  
  vector[nfirstobs] first;
  
  real<lower=0> sdnoise;
  vector[ncounts] noise_raw;
  vector[30] new_noise_raw;
  

}

transformed parameters{
  
 vector[ncounts] lambda;


  for(i in 1:ncounts){
  real noise = sdnoise*noise_raw[i];
  
  lambda[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] + b_space[reg[i]] * space[i] * pforest[i] + log(stops[i]) + observer[obs[i]] + first[firstobs[i]] + noise;
  
  }
  
    // likelihood
}



model {

  
 
// MAIN MODEL

 to_vector(a) ~ normal(0, 1);

 b_space ~ normal(0, 1);
 b_time ~ normal(0, 1);

 observer ~ normal(0, sdobs);
 first ~ normal(0, sdfirst);
 sdobs ~ normal(0, 1);
 sdfirst ~ normal(0, 1);
 
 sdnoise ~ normal(0,1);
 noise_raw ~ normal(0, 1);
 new_noise_raw ~ normal(0, 1);


richness ~ poisson_log(lambda);         
   
}

generated quantities{
    int y_rep[ncounts];
  //vector[ncounts] log_lik;
  vector[nreg]  b_dif_rg;
  //real<lower=0> retrans_noise;
  //real<lower=0> retrans_obs;
  matrix[nreg, ncounts] y_space;
  matrix[nreg, ncounts] y_time;
  
  //retrans_noise = 0.5*(sdnoise^2);
  //retrans_obs = 0.5*(sdobs^2);


     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
     
    // Y_rep for prior predictive check
  for(i in 1:30){
  
    real new_noise = sdnoise*new_noise_raw[i];
    
    for(n in 1:nreg){
      
      y_space[n,i] = poisson_log_rng(a[n, 2] + b_time[n] * 0 * xseq[i] + b_space[n] * 1 * xseq[i] + new_noise);
        

      
      y_time[n,i] = poisson_log_rng(a[n, 1] + b_time[n] * 1 * xseq[i] + b_space[n] * 0 * xseq[i] + new_noise);

  }
  }
  // Y_rep for prior predictive check
  for(k in 1:ncounts){
  y_rep[k] = poisson_log_rng(lambda[k]);
  }
  
  
  // for(n in 1:ncounts){
    // log_lik[n] = poisson_lcdf(richness[n] | a[reg[n], spacetime[n]] + b_time[reg[n]] * time[n] * pforest[n] +  b_space[reg[n]] * space[n] * pforest[n] + observer[obs[n]] + log(stops[n]) + first[firstobs[n]] + sdnoise[n]);
    
  // }
  
  
  
  
}
