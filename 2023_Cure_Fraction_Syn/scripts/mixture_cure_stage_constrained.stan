//
// This Stan program defines a mixture cure model across stages
// vector of values of survival
// parameters constrained to always be worse per stage
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> Q;
  int<lower=0> J;
  real <lower=0,upper=1> upper_decay;
  int stage[N];
  vector[N] ax;
  vector[N] alive;
  vector[N] died;
  vector[N] lost;
  vector[N] css;
}

// The parameters accepted by the model. Our model
// accepts four parameters for a mixture model.
// a_weibull is the usual weibull parameter
// y_weibull is the ratio of survivals at time ax[Q] which must decreasse
// xc_fraction is the ratio of cure fraction which must decrease
// xd_decay is the reversed ratio of recurrence which must increase
parameters {
  real <lower=0.5,upper=2> a_weibull[J];
  //real <lower=-8,upper=2> b_weibull[J];
  real <lower=0,upper=1> y_weibull[J];
  real <lower=0,upper=1> xc_fraction[J];
  real <lower=0,upper=1> xd_decay[J];
}
// cure fraction decreases with stage,
// d_decay increases with stage
// b_weibull makes sure survival at long times decreases by stage in not-cured
transformed parameters{
  real <lower=0,upper=1> c_fraction[J];
  real <lower=0,upper=1> d_decay[J];
  real <lower=-9,upper=2> b_weibull[J];
  real tmp_c;
  real tmp_d;
  real tmp_b;
  tmp_c=1.0;
  tmp_d=upper_decay;
  for (jj in 1:J){
    tmp_c=tmp_c*xc_fraction[jj]; // cumulative product to assure decrease by stage
    c_fraction[jj]=tmp_c;
    tmp_d=tmp_d*xd_decay[J-jj+1]; // cumulative product to assure increase by stage, reverse index
    d_decay[J-jj+1]=tmp_d;
  }
  // weibull parameter
  // survival at ax[Q] must decrease as stage increases in those not cured
  tmp_b=1.0;
  for (jj in 1:J){
    tmp_b=tmp_b*y_weibull[jj];
    b_weibull[jj]=log(-log(tmp_b))-a_weibull[jj]*log(ax[Q]);
  }
}
// The model to be estimated. We model the output
// predicted survival and compute likelihood from the life table
// 
model {
  real tmp_lt;
  real pred_nc;
  real pred_c;
  real pred_s;
  real prev_s;
   int i;
 for (j in 1:J){
    prev_s=1.0;
     for (iq in 1:Q){
       i=j*Q-Q+iq;
      tmp_lt=log(ax[i]); // log
      pred_nc=exp(-exp(a_weibull[j]*tmp_lt+b_weibull[j]));
      pred_c=exp(-d_decay[j]*ax[i]);
      pred_s=(1.0-c_fraction[j])*pred_nc+c_fraction[j]*pred_c;
      // now compute individual outcomes and apply
      // died are in difference in survival between last time point seen and current end-of-interval timepoint
      target += died[i]*log(prev_s-pred_s);
      //lost have survived up to last time point seen
      target += lost[i]*log(prev_s);
      prev_s=pred_s;
      //target += normal_lpdf(css[i] | pred_s,0.05);
    }
    // add survivors at last known survival point
    target += (alive[i]-lost[i]-died[i])*log(pred_s);
    // if we care we can adjust the target by the jacobian of the transformation
    // however, since we are mainly using it as a constraint this is not very useful
  }
}

