#include <RcppArmadillo.h>
#include "methods.h"
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


//' @title Likelihood ratio test statistic sample distribution
//' 
//' @description This function is used by Monte Carlo Likelihood ratio testing 
//' procedures to simulate processes under the null hypothesis.
//' 
//' @param mdl_h0 List with restricted model properties.
//' @param p integer specifying the number of autoregressive lags.
//' @param q integer specifying the number of series.
//' @param k integer specifying the number of regimes.
//' @param burnin integer specifying the number of observations to drop from beginning of simulation.
//' 
//' @return List with simulated process properties. 
//' 
//' @keywords internal
//' 
//' @export
// [[Rcpp::export]]
List simuMdl(List mdl_h0, int p, int q, int k, int burnin){
  // ---------- Load R functions
  Rcpp::Environment mstest("package:MSTest");
  Rcpp::Function MSARmdl = mstest["MSARmdl"];
  Rcpp::Function MSVARmdl = mstest["MSVARmdl"];
  Rcpp::Function ARmdl = mstest["ARmdl"];
  Rcpp::Function VARmdl = mstest["VARmdl"];
  Rcpp::Function Nmdl = mstest["Nmdl"];
  Rcpp::Function HMmdl = mstest["HMmdl"];
  List simu_mdl;
  if ((k==1) & (p==0)){
    simu_mdl = simuNorm(mdl_h0, burnin);
  }else if ((k>1) & (p==0)){
    // Hidden Markov model
    simu_mdl = simuHMM(mdl_h0, burnin);
  }else if ((k==1) & (q==1) & (p>0)){
    // Autoregressive model
    simu_mdl = simuAR(mdl_h0, burnin);
  }else if ((k>1) & (q==1) & (p>0)){
    // Markov switching model
    simu_mdl = simuMSAR(mdl_h0, burnin);
  }else if ((k==1) & (q>1) & (p>0)){
    // Vector autoregressive model
    simu_mdl = simuVAR(mdl_h0, burnin);
  }else if ((k>1) & (q>1) & (p>0)){
    // Vector autoregressive Markov switching model
    simu_mdl = simuMSVAR(mdl_h0, burnin);
  }
  return(simu_mdl);
}




//' @title Change model List with new parameters
//' 
//' @description This function is used by the MMC LRT procedure. It makes a 
//' copy of the restricted model under the null hypothesis and changes the 
//' parameters used to simulate the process under the null with values being 
//' considered in the search/optimization. 
//' 
//' @param mdl_h0 List with restricted model properties.
//' @param theta_h0 vector of parameter values under null being considered.
//' @param p integer specifying the number of autoregressive lags.
//' @param q integer specifying the number of series.
//' @param k integer specifying the number of regimes.
//' 
//' @return List with model properties
//' 
//' @keywords internal
//' 
//' @export
// [[Rcpp::export]]
List mdledit(List mdl_h0, arma::vec theta_h0, int p, int q, int k0){
  List mdl_h0_tmp = clone(mdl_h0);
  // edit theta vector
  mdl_h0_tmp["theta"] = theta_h0;
  arma::vec theta_mu_ind_h0 = mdl_h0["theta_mu_ind"];
  arma::vec theta_mu_h0 = theta_h0.elem(find(theta_mu_ind_h0));
  arma::vec theta_sig_ind_h0 = mdl_h0["theta_sig_ind"];
  arma::vec theta_sig_h0 = theta_h0.elem(find(theta_sig_ind_h0));
  // edit simulation list params with values from theta_0
  if (q==1){
    mdl_h0_tmp["mu"] = theta_mu_h0;
    mdl_h0_tmp["stdev"] = sqrt(theta_sig_h0);
    if (p>0){
      arma::vec theta_phi_ind_h0 = mdl_h0["theta_phi_ind"];
      arma::vec phi_new = theta_h0.elem(find(theta_phi_ind_h0));
      mdl_h0_tmp["phi"] = phi_new;
    }
  }else if (q>1){
    arma::mat mu_new = trans(reshape(theta_mu_h0,q,k0));
    mdl_h0_tmp["mu"] = mu_new;
    if (k0==1){
      mdl_h0_tmp["sigma"] = covar_unvech(theta_sig_h0,q);
    }else if (k0>1){
      arma::mat sig_new_tmp = trans(reshape(theta_sig_h0, (q*(q+1))/2, k0));
      List sig_new(k0);
      for (int xk = 0; xk<k0; xk++){
        arma::vec sig_tmp_k = trans(sig_new_tmp.row(xk));
        sig_new[xk] = covar_unvech(sig_tmp_k, q);
      }
      mdl_h0_tmp["sigma"] = sig_new;
    } 
    if (p>0){
      arma::vec theta_phi_ind_h0 = mdl_h0["theta_phi_ind"];
      // phi vectors
      arma::vec phi_vec_h0 = theta_h0.elem(find(theta_phi_ind_h0));
      // phi matrices
      arma::mat phi_h0 = trans(reshape(phi_vec_h0, q*p, q));
      mdl_h0_tmp["phi"] = phi_h0;
    } 
  }
  return(mdl_h0_tmp);
}

//' @title Computes test stat using new parameter vectors
//' 
//' @description This function computes the LRT statistic.
//' 
//' @param theta_h0 estimated parameter values for restricted model.
//' @param theta_h1 estimated parameter values for unrestricted model.
//' @param mdl_h0 List with restricted model properties.
//' @param mdl_h1 List with unrestricted model properties.
//' @param p integer specifying the number of autoregressive lags.
//' @param q integer specifying the number of series.
//' @param k0 integer specifying the number of regimes under the null hypothesis.
//' @param k1 integer specifying the number of regimes under the alternative hypothesis.
//' 
//' @return LRT statistic
//' 
//' @keywords internal
//' 
//' @export
// [[Rcpp::export]]
double compu_tstat(arma::vec theta_h0, arma::vec theta_h1, 
                   List mdl_h0, List mdl_h1, int p, int q, int k0, int k1){
  double logL0;
  double logL1;
  // compute test stat
  if ((k0==1) & (p==0)){
    // Nmdl under null & HMmdl under alternative hypothesis
    logL0 = logLike_Nmdl(theta_h0, mdl_h0);  
    logL1 = logLike_HMmdl(theta_h1, mdl_h1, k1);
  }else if ((k0>1) & (p==0)){
    // HMmdl under null & alternative hypothesis
    logL0 = logLike_HMmdl(theta_h0, mdl_h0, k0);  
    logL1 = logLike_HMmdl(theta_h1, mdl_h1, k1);
  }else if ((k0==1) & (q==1) & (p>0)){
    // ARmdl under null & MSARmdl under alternative hypothesis
    logL0 = logLike_ARmdl(theta_h0, mdl_h0);  
    logL1 = logLike_MSARmdl(theta_h1, mdl_h1, k1);
  }else if ((k0>1) & (q==1) & (p>0)){
    // MSARmdl under null & alternative hypothesis
    logL0 = logLike_MSARmdl(theta_h0, mdl_h0, k0);  
    logL1 = logLike_MSARmdl(theta_h1, mdl_h1, k1);
  }else if ((k0==1) & (q>1) & (p>0)){
    // VARmdl under null & MSVARmdl under alternative hypothesis
    logL0 = logLike_VARmdl(theta_h0, mdl_h0);  
    logL1 = logLike_MSVARmdl(theta_h1, mdl_h1, k1);
  }else if ((k0>1) & (q>1) & (p>0)){
    // MSARmdl under null & alternative hypothesis
    logL0 = logLike_MSVARmdl(theta_h0, mdl_h0, k0);  
    logL1 = logLike_MSVARmdl(theta_h1, mdl_h1, k1);
  }else{
    stop("Verify number of regimes under null and alternative");
  }
  double LRT_0 = -2*(logL0-logL1);
  return(LRT_0);
}




//' @title Likelihood Ratio Test Statistic Sample Distribution
//' 
//' @description This function simulates the sample distribution under the null hypothesis.
//' 
//' @param mdl_h0 List with restricted model properties.
//' @param k1 integer specifying the number of regimes under the alternative hypothesis.
//' @param N integer specifying the number of replications.
//' @param burnin integer specifying the number of observations to drop from beginning of simulation.
//' @param mdl_h0_control List with controls/options used to estimate restricted model.
//' @param mdl_h1_control List with controls/options used to estimate unrestricted model.
//' 
//' @return vector of simulated LRT statistics
//' 
//' @keywords internal
//' 
//' @references Rodriguez Rondon, Gabriel and Jean-Marie Dufour. 2022. "Simulation-Based Inference for Markov Switching Models” \emph{JSM Proceedings, Business and Economic Statistics Section: American Statistical Association}.
//' @references Rodriguez Rondon, Gabriel and Jean-Marie Dufour. 2022. “Monte Carlo Likelihood Ratio Tests for Markov Switching Models.” \emph{Unpublished manuscript}.
//' 
//' @export
// [[Rcpp::export]]
arma::vec LR_samp_dist(List mdl_h0, int k1, int N, int burnin, 
                       List mdl_h0_control, List mdl_h1_control){
  // ---------- Load R functions
  Rcpp::Environment mstest("package:MSTest");
  Rcpp::Function estimMdl = mstest["estimMdl"];
  // ---------- Define required parameters
  int k0  = mdl_h0["k"];
  int p   = mdl_h0["p"];
  int q   = mdl_h0["q"];
  //  Fix 'getSE' options to be FALSE as it is not needed for simulated series and slows down computation
  bool getSE = FALSE;
  List mdl_h0_control_tmp = clone(mdl_h0_control);
  List mdl_h1_control_tmp = clone(mdl_h1_control);
  mdl_h0_control_tmp["getSE"] = getSE;
  mdl_h1_control_tmp["getSE"] = getSE;
  // ---------- Simulate test statistic under null hypothesis
  double LRT_i;
  arma::vec LRT_N(N,arma::fill::zeros);
  for (int xn = 0; xn<N; xn++){
    bool LRT_finite   = FALSE; 
    while (LRT_finite==FALSE){
      List simu_mdl   = simuMdl(mdl_h0, p, q, k0, burnin);
      arma::mat y0    = simu_mdl["y"];
      List mdl_h0_tmp = estimMdl(y0, p, q, k0, mdl_h0_control_tmp);
      List mdl_h1_tmp = estimMdl(y0, p, q, k1, mdl_h1_control_tmp);
      double l_0      = mdl_h0_tmp["logLike"];
      double l_1      = mdl_h1_tmp["logLike"];
      LRT_i = -2*(l_0 - l_1);
      // verify test stat (i.e. not NaN nor <0)
      LRT_finite = ((arma::is_finite(LRT_i)) and (LRT_i>=0));
    }
    LRT_N(xn) = LRT_i;
  }
  return(LRT_N);
}

//' @title Monte Carlo Likelihood Ratio Test P-value Function 
//' 
//' @description This function computes the Maximum Monte Carlo P-value.
//' 
//' @param theta vector of parameter values being considered.
//' @param mdl_h0 List with restricted model properties.
//' @param mdl_h1 List with unrestricted model properties.
//' @param N integer specifying the number of replications.
//' @param burnin integer specifying the number of observations to drop from beginning of simulation.
//' @param workers Integer determining the number of workers to use for parallel computing version of test. Note that parallel pool must already be open.
//' @param lambda Double determining penalty on nonlinear constraint.
//' @param stationary_constraint Boolean determining if only stationary solutions are considered (if \code{TRUE}) or not (if \code{FALSE}).
//' @param thtol double determining the convergence criterion used during estimation.
//' @param mdl_h0_control List with controls/options used to estimate restricted model.
//' @param mdl_h1_control List with controls/options used to estimate unrestricted model.
//' 
//' @return MMC p-value
//' 
//' @keywords internal
//' 
//' @export
// [[Rcpp::export]]
double MMCLRpval_fun(arma::vec theta, List mdl_h0, List mdl_h1, int N, 
                     int burnin, int workers, double lambda, 
                     bool stationary_constraint, double thtol,   
                     List mdl_h0_control, List mdl_h1_control){
  Rcpp::Environment mstest("package:MSTest");
  Rcpp::Function LR_samp_dist_par = mstest["LR_samp_dist_par"];
  // ----- define required variables from inputs
  int k0 = mdl_h0["k"];
  int k1 = mdl_h1["k"];
  int q = mdl_h0["q"];
  int p = mdl_h0["p"];
  arma::vec theta_h0_tmp = mdl_h0["theta"];
  arma::vec theta_h1_tmp = mdl_h1["theta"];
  int theta_h0_length = theta_h0_tmp.n_elem;
  int theta_h1_length = theta_h1_tmp.n_elem;
  arma::vec theta_h0 = theta.subvec(0,theta_h0_length-1);
  arma::vec theta_h1 = theta.subvec(theta_h0_length,theta_h0_length+theta_h1_length-1);
  // ----- initialize variables 
  double pval;
  arma::mat P_h0;
  arma::mat P_h1;
  bool non_stationary_const = FALSE;
  bool P_h0_colsum_const = FALSE;
  bool P_h1_colsum_const = FALSE;
  // ----- Stationary constraint (i.e., only consider theta that result in stationary process) 
  if ((stationary_constraint==TRUE) and (p>0) and (q==1)){
    Rcpp::Function polyroot("polyroot");  
    Rcpp::Function Mod("Mod");  
    arma::vec theta_phi_h0 = mdl_h0["theta_phi_ind"];
    arma::vec theta_phi_h1 = mdl_h1["theta_phi_ind"];
    arma::vec poly_fun_h0(p+1, arma::fill::ones);
    arma::vec poly_fun_h1(p+1, arma::fill::ones);
    poly_fun_h0.subvec(1,p) =  -theta_h0(find(theta_phi_h0==1));
    poly_fun_h1.subvec(1,p) =  -theta_h1(find(theta_phi_h1==1));
    arma::vec roots_h0 = as<arma::vec>(Mod(wrap(as<ComplexVector>(polyroot(wrap(poly_fun_h0))))));
    arma::vec roots_h1 = as<arma::vec>(Mod(wrap(as<ComplexVector>(polyroot(wrap(poly_fun_h1))))));
    non_stationary_const = ((roots_h0.min()<=1) or (roots_h1.min()<=1));
  }else if ((stationary_constraint==TRUE) and (p>0) and (q>1)){
    Rcpp::Function Mod("Mod");  
    // phi indicators
    arma::vec theta_phi_ind_h0 = mdl_h0["theta_phi_ind"];
    arma::vec theta_phi_ind_h1 = mdl_h1["theta_phi_ind"];
    // phi vectors
    arma::vec phi_vec_h0 = theta_h0.elem(find(theta_phi_ind_h0));
    arma::vec phi_vec_h1 = theta_h1.elem(find(theta_phi_ind_h1)); 
    // phi matrices
    arma::mat phi_h0 = reshape(phi_vec_h0, q*p, q);
    arma::mat phi_h1 = reshape(phi_vec_h1, q*p, q);
    // companion matrices
    arma::mat F0_tmp = trans(phi_h0);
    arma::mat F1_tmp = trans(phi_h1);
    arma::mat diagmat = arma::eye(q*(p-1),q*(p-1));
    arma::mat diagzero(q*(p-1), q, arma::fill::zeros);
    arma::mat Mn = join_rows(diagmat,diagzero);
    arma::mat F0 = join_cols(F0_tmp,Mn);
    arma::mat F1 = join_cols(F1_tmp,Mn);
    // eigen values
    arma::cx_vec eig_0 = eig_gen(F0);
    arma::cx_vec eig_1 = eig_gen(F1);
    bool stationary_0 = all(as<arma::vec>(Mod(eig_0))<1);
    bool stationary_1 = all(as<arma::vec>(Mod(eig_1))<1);
    non_stationary_const = ((stationary_0==FALSE) or (stationary_1==FALSE));
  }
  // ----- Checking that P matrix columns sum to 1
  if (k0>1){
    arma::vec theta_P_ind_h0 = mdl_h0["theta_P_ind"];
    arma::vec P_vec_h0 = theta_h0.elem(find(theta_P_ind_h0));
    P_h0 = reshape(P_vec_h0, k0, k0);
    P_h0_colsum_const = any(abs(arma::sum(P_h0,0)-1)>thtol); 
  }
  arma::vec theta_P_ind_h1 = mdl_h1["theta_P_ind"];
  arma::vec P_vec_h1 = theta_h1.elem(find(theta_P_ind_h1));
  P_h1 = reshape(P_vec_h1, k1, k1); 
  P_h1_colsum_const = any(abs(arma::sum(P_h1,0)-1)>thtol);
  // ----- Compute pval 
  if ((P_h0_colsum_const==TRUE) or (P_h1_colsum_const==TRUE) or (non_stationary_const==TRUE)){
    // If either transition matrix columns do not sum to 1 OR (stationary_constraint == TRUE AND non_stationary_const == TRUE), pval is a negative constant
    pval = -(lambda*(P_h0_colsum_const + P_h1_colsum_const + non_stationary_const));
  }else{
    // If constraints are met, compute pvalue
    List mdl_h0_tmp = mdledit(mdl_h0, theta_h0, p, q, k0);
    double LRT_0 = compu_tstat(theta_h0, theta_h1, mdl_h0, mdl_h1, p, q, k0, k1);
    // simulate under null hypothesis
    arma::vec LRN_tmp;
    if(workers>0){
      LRN_tmp = as<arma::vec>(LR_samp_dist_par(mdl_h0_tmp, k1, N, burnin, mdl_h0_control, mdl_h1_control, workers));
    }else{
      LRN_tmp = LR_samp_dist(mdl_h0_tmp, k1, N, burnin, mdl_h0_control, mdl_h1_control);
    }
    pval = MCpval(LRT_0, LRN_tmp, "geq");
  }
  return(pval);
}


//' @title Monte Carlo Likelihood Ratio Test P-value Function 
//' 
//' @description This function computes the (negative) Maximum Monte Carlo P-value.
//' 
//' @param theta vector of parameter values being considered.
//' @param mdl_h0 List with restricted model properties.
//' @param mdl_h1 List with unrestricted model properties.
//' @param N integer specifying the number of replications.
//' @param burnin integer specifying the number of observations to drop from beginning of simulation.
//' @param workers Integer determining the number of workers to use for parallel computing version of test. Note that parallel pool must already be open.
//' @param lambda Double determining penalty on nonlinear constraint.
//' @param stationary_constraint Boolean determining if only stationary solutions are considered (if \code{TRUE}) or not (if \code{FALSE}).
//' @param thtol double determining the convergence criterion used during estimation.
//' @param mdl_h0_control List with controls/options used to estimate restricted model.
//' @param mdl_h1_control List with controls/options used to estimate unrestricted model.
//' 
//' @return negative MMC p-value
//' 
//' @keywords internal
//' 
//' @export
// [[Rcpp::export]]
double MMCLRpval_fun_min(arma::vec theta, List mdl_h0, List mdl_h1, int N, int burnin, int workers, 
                         double lambda, bool stationary_constraint, double thtol,   
                         List mdl_h0_control, List mdl_h1_control){
  double pval = -MMCLRpval_fun(theta, mdl_h0, mdl_h1, N, burnin, workers, lambda, stationary_constraint, thtol, mdl_h0_control, mdl_h1_control);     
  return(pval);
}


