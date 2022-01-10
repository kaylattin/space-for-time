

data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  int<lower=1> nst;
  int<lower=1> nfirstobs;
  int<lower=1> nstops;

  int<lower=1> spacetime[ncounts];
  int<lower=0> richness[ncounts];   // Species richness as a count/integer
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int<lower=1> reg[ncounts];  // Regions
  int<lower=1> stops[ncounts];

  int<lower=1> obs[ncounts];   // observers
  int<lower=1> firstobs[ncounts];
  real pforest[ncounts];  // Percent forest cover
  

}



parameters {
// MAIN MODEL
  matrix[nreg, nst] a;

  vector[nreg] b_space;
  vector[nreg] b_time;
  
  vector[nobs] observer;
  
  vector[nfirstobs] first;
  
  vector<lower=0>[ncounts] sdnoise;

}



model {
   vector[ncounts] lambda;

  
 
// MAIN MODEL

 to_vector(a) ~ normal(0, 1);

 b_space ~ normal(0, 1);
 b_time ~ normal(0, 1);

 observer ~ normal(0, 1);
 first ~ normal(0, 1);
 
 sdnoise ~ student_t(1, 0, 4);


  
  // likelihood
    for(i in 1:ncounts) {
      
    lambda[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] + log(stops[i]) + b_space[reg[i]] * space[i] * pforest[i] + observer[obs[i]] + first[firstobs[i]] + sdnoise[i];
    
    }



richness ~ poisson_log(lambda);         
   
}

generated quantities{
  int y_rep[ncounts];
  vector[ncounts] log_lik;
  vector[nreg]  b_dif_rg;

     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
  


  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = poisson_log_rng(a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] +  b_space[reg[i]] * space[i] * pforest[i] + observer[obs[i]] + log(stops[i]) + first[firstobs[i]] + sdnoise[i]);
  }
  
  
  // for(n in 1:ncounts){
    // log_lik[n] = poisson_lcdf(richness[n] | a[reg[n], spacetime[n]] + b_time[reg[n]] * time[n] * pforest[n] +  b_space[reg[n]] * space[n] * pforest[n] + observer[obs[n]] + log(stops[n]) + first[firstobs[n]] + sdnoise[n]);
    
  // }
  
  
  
  
}
