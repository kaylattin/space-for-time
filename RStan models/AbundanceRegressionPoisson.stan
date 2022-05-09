

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
  int<lower=0> ta[ncounts];   // Species ta as a count/integer
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
  
  vector[nobs] observer_raw;
  vector[nfirstobs] first_raw;
  real<lower=0> sdobs;
  real<lower=0> sdfirst;
  

  
  real<lower=0> sdnoise;
  vector[ncounts] noise_raw;
  vector[30] new_noise_raw;
  

}

transformed parameters{
  
 vector[ncounts] lambda;
 vector[nobs] observer;
 vector[nfirstobs] first;

  observer = sdobs * observer_raw; 
  first = sdobs * first_raw;


  for(i in 1:ncounts){
  real noise = sdnoise*noise_raw[i];
  
  lambda[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] + log(stops[i]) + b_space[reg[i]] * space[i] * pforest[i] + observer[obs[i]] + first[firstobs[i]] + noise;
  
  }
  
    // likelihood
}



model {

  
 
// MAIN MODEL

 to_vector(a) ~ std_normal();

 b_space ~ std_normal();
 b_time ~ std_normal();

 observer_raw ~ std_normal();
 first_raw ~ std_normal();
 
 sdobs ~ std_normal();
 sdfirst ~ std_normal();
 
 sdnoise ~ std_normal();
 noise_raw ~ std_normal();
 new_noise_raw ~ std_normal();

ta ~ poisson_log(lambda);         
   
}

generated quantities{
  int y_rep[ncounts];
  vector[nreg]  b_dif_rg;
  real<lower=0> retrans_noise;
  real<lower=0> retrans_obs;

  retrans_noise = 0.5*(sdnoise^2);
  retrans_obs = 0.5*(sdobs^2);


     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
  
  
    // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = poisson_log_rng(lambda[i]);
  }

  
}
