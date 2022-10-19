// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cov2corr
arma::mat cov2corr(arma::mat cov_mat);
RcppExport SEXP _MSTest_cov2corr(SEXP cov_matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type cov_mat(cov_matSEXP);
    rcpp_result_gen = Rcpp::wrap(cov2corr(cov_mat));
    return rcpp_result_gen;
END_RCPP
}
// covar_vech
arma::vec covar_vech(arma::mat mat);
RcppExport SEXP _MSTest_covar_vech(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(covar_vech(mat));
    return rcpp_result_gen;
END_RCPP
}
// covar_unvech
arma::mat covar_unvech(arma::vec sig, int n);
RcppExport SEXP _MSTest_covar_unvech(SEXP sigSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(covar_unvech(sig, n));
    return rcpp_result_gen;
END_RCPP
}
// randP
arma::mat randP(int k);
RcppExport SEXP _MSTest_randP(SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(randP(k));
    return rcpp_result_gen;
END_RCPP
}
// limP
arma::vec limP(arma::mat P);
RcppExport SEXP _MSTest_limP(SEXP PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type P(PSEXP);
    rcpp_result_gen = Rcpp::wrap(limP(P));
    return rcpp_result_gen;
END_RCPP
}
// ts_lagged
List ts_lagged(arma::mat Y, int p);
RcppExport SEXP _MSTest_ts_lagged(SEXP YSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(ts_lagged(Y, p));
    return rcpp_result_gen;
END_RCPP
}
// paramList_MSARmdl
List paramList_MSARmdl(arma::vec theta, int p, int k, bool msmu, bool msvar);
RcppExport SEXP _MSTest_paramList_MSARmdl(SEXP thetaSEXP, SEXP pSEXP, SEXP kSEXP, SEXP msmuSEXP, SEXP msvarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type msmu(msmuSEXP);
    Rcpp::traits::input_parameter< bool >::type msvar(msvarSEXP);
    rcpp_result_gen = Rcpp::wrap(paramList_MSARmdl(theta, p, k, msmu, msvar));
    return rcpp_result_gen;
END_RCPP
}
// paramList_MSVARmdl
List paramList_MSVARmdl(arma::vec theta, int q, int p, int k, bool msmu, bool msvar);
RcppExport SEXP _MSTest_paramList_MSVARmdl(SEXP thetaSEXP, SEXP qSEXP, SEXP pSEXP, SEXP kSEXP, SEXP msmuSEXP, SEXP msvarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type msmu(msmuSEXP);
    Rcpp::traits::input_parameter< bool >::type msvar(msvarSEXP);
    rcpp_result_gen = Rcpp::wrap(paramList_MSVARmdl(theta, q, p, k, msmu, msvar));
    return rcpp_result_gen;
END_RCPP
}
// calcResid_MSARmdl
arma::mat calcResid_MSARmdl(List mdl, arma::mat mu, int k);
RcppExport SEXP _MSTest_calcResid_MSARmdl(SEXP mdlSEXP, SEXP muSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(calcResid_MSARmdl(mdl, mu, k));
    return rcpp_result_gen;
END_RCPP
}
// calcResid_MSVARmdl
List calcResid_MSVARmdl(List mdl, List mu, int k);
RcppExport SEXP _MSTest_calcResid_MSVARmdl(SEXP mdlSEXP, SEXP muSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< List >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(calcResid_MSVARmdl(mdl, mu, k));
    return rcpp_result_gen;
END_RCPP
}
// initVals_HMmdl
arma::vec initVals_HMmdl(List mdl, int k);
RcppExport SEXP _MSTest_initVals_HMmdl(SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(initVals_HMmdl(mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// initVals_MSARmdl
arma::vec initVals_MSARmdl(List mdl, int k);
RcppExport SEXP _MSTest_initVals_MSARmdl(SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(initVals_MSARmdl(mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// initVals_MSVARmdl
arma::vec initVals_MSVARmdl(List mdl, int k);
RcppExport SEXP _MSTest_initVals_MSVARmdl(SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(initVals_MSVARmdl(mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// MCpval
double MCpval(double test_stat, arma::vec null_vec, Rcpp::String type);
RcppExport SEXP _MSTest_MCpval(SEXP test_statSEXP, SEXP null_vecSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type test_stat(test_statSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type null_vec(null_vecSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(MCpval(test_stat, null_vec, type));
    return rcpp_result_gen;
END_RCPP
}
// randSN
arma::mat randSN(int n, int q);
RcppExport SEXP _MSTest_randSN(SEXP nSEXP, SEXP qSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    rcpp_result_gen = Rcpp::wrap(randSN(n, q));
    return rcpp_result_gen;
END_RCPP
}
// simuAR
List simuAR(List mdl_h0, int burnin);
RcppExport SEXP _MSTest_simuAR(SEXP mdl_h0SEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(simuAR(mdl_h0, burnin));
    return rcpp_result_gen;
END_RCPP
}
// simuMSAR
List simuMSAR(List mdl_h0, int burnin);
RcppExport SEXP _MSTest_simuMSAR(SEXP mdl_h0SEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(simuMSAR(mdl_h0, burnin));
    return rcpp_result_gen;
END_RCPP
}
// simuVAR
List simuVAR(List mdl_h0, int burnin);
RcppExport SEXP _MSTest_simuVAR(SEXP mdl_h0SEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(simuVAR(mdl_h0, burnin));
    return rcpp_result_gen;
END_RCPP
}
// simuMSVAR
List simuMSVAR(List mdl_h0, int burnin);
RcppExport SEXP _MSTest_simuMSVAR(SEXP mdl_h0SEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(simuMSVAR(mdl_h0, burnin));
    return rcpp_result_gen;
END_RCPP
}
// simuNorm
List simuNorm(List mdl_h0, int burnin);
RcppExport SEXP _MSTest_simuNorm(SEXP mdl_h0SEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(simuNorm(mdl_h0, burnin));
    return rcpp_result_gen;
END_RCPP
}
// simuHMM
List simuHMM(List mdl_h0, int burnin);
RcppExport SEXP _MSTest_simuHMM(SEXP mdl_h0SEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(simuHMM(mdl_h0, burnin));
    return rcpp_result_gen;
END_RCPP
}
// logLike_Nmdl
double logLike_Nmdl(arma::vec theta, List mdl);
RcppExport SEXP _MSTest_logLike_Nmdl(SEXP thetaSEXP, SEXP mdlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_Nmdl(theta, mdl));
    return rcpp_result_gen;
END_RCPP
}
// logLike_ARmdl
double logLike_ARmdl(arma::vec theta, List mdl);
RcppExport SEXP _MSTest_logLike_ARmdl(SEXP thetaSEXP, SEXP mdlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_ARmdl(theta, mdl));
    return rcpp_result_gen;
END_RCPP
}
// logLike_VARmdl
double logLike_VARmdl(arma::vec theta, List mdl);
RcppExport SEXP _MSTest_logLike_VARmdl(SEXP thetaSEXP, SEXP mdlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_VARmdl(theta, mdl));
    return rcpp_result_gen;
END_RCPP
}
// logLike_HMmdl
double logLike_HMmdl(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_logLike_HMmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_HMmdl(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// logLike_HMmdl_min
double logLike_HMmdl_min(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_logLike_HMmdl_min(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_HMmdl_min(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// logLike_MSARmdl
double logLike_MSARmdl(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_logLike_MSARmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_MSARmdl(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// logLike_MSARmdl_min
double logLike_MSARmdl_min(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_logLike_MSARmdl_min(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_MSARmdl_min(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// logLike_MSVARmdl
double logLike_MSVARmdl(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_logLike_MSVARmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_MSVARmdl(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// logLike_MSVARmdl_min
double logLike_MSVARmdl_min(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_logLike_MSVARmdl_min(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_MSVARmdl_min(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// ExpectationM_HMmdl
List ExpectationM_HMmdl(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_ExpectationM_HMmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(ExpectationM_HMmdl(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// ExpectationM_MSARmdl
List ExpectationM_MSARmdl(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_ExpectationM_MSARmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(ExpectationM_MSARmdl(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// ExpectationM_MSVARmdl
List ExpectationM_MSVARmdl(arma::vec theta, List mdl, int k);
RcppExport SEXP _MSTest_ExpectationM_MSVARmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(ExpectationM_MSVARmdl(theta, mdl, k));
    return rcpp_result_gen;
END_RCPP
}
// EMaximization_HMmdl
List EMaximization_HMmdl(arma::vec theta, List mdl, List MSloglik_output, int k);
RcppExport SEXP _MSTest_EMaximization_HMmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP MSloglik_outputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< List >::type MSloglik_output(MSloglik_outputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(EMaximization_HMmdl(theta, mdl, MSloglik_output, k));
    return rcpp_result_gen;
END_RCPP
}
// EMaximization_MSARmdl
List EMaximization_MSARmdl(arma::vec theta, List mdl, List MSloglik_output, int k);
RcppExport SEXP _MSTest_EMaximization_MSARmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP MSloglik_outputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< List >::type MSloglik_output(MSloglik_outputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(EMaximization_MSARmdl(theta, mdl, MSloglik_output, k));
    return rcpp_result_gen;
END_RCPP
}
// EMaximization_MSVARmdl
List EMaximization_MSVARmdl(arma::vec theta, List mdl, List MSloglik_output, int k);
RcppExport SEXP _MSTest_EMaximization_MSVARmdl(SEXP thetaSEXP, SEXP mdlSEXP, SEXP MSloglik_outputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< List >::type MSloglik_output(MSloglik_outputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(EMaximization_MSVARmdl(theta, mdl, MSloglik_output, k));
    return rcpp_result_gen;
END_RCPP
}
// EMiter_HMmdl
List EMiter_HMmdl(List mdl, List EMest_output, int k);
RcppExport SEXP _MSTest_EMiter_HMmdl(SEXP mdlSEXP, SEXP EMest_outputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< List >::type EMest_output(EMest_outputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(EMiter_HMmdl(mdl, EMest_output, k));
    return rcpp_result_gen;
END_RCPP
}
// EMiter_MSARmdl
List EMiter_MSARmdl(List mdl, List EMest_output, int k);
RcppExport SEXP _MSTest_EMiter_MSARmdl(SEXP mdlSEXP, SEXP EMest_outputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< List >::type EMest_output(EMest_outputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(EMiter_MSARmdl(mdl, EMest_output, k));
    return rcpp_result_gen;
END_RCPP
}
// EMiter_MSVARmdl
List EMiter_MSVARmdl(List mdl, List EMest_output, int k);
RcppExport SEXP _MSTest_EMiter_MSVARmdl(SEXP mdlSEXP, SEXP EMest_outputSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< List >::type EMest_output(EMest_outputSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(EMiter_MSVARmdl(mdl, EMest_output, k));
    return rcpp_result_gen;
END_RCPP
}
// HMmdl_em
List HMmdl_em(arma::vec theta_0, List mdl, int k, List optim_options);
RcppExport SEXP _MSTest_HMmdl_em(SEXP theta_0SEXP, SEXP mdlSEXP, SEXP kSEXP, SEXP optim_optionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta_0(theta_0SEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< List >::type optim_options(optim_optionsSEXP);
    rcpp_result_gen = Rcpp::wrap(HMmdl_em(theta_0, mdl, k, optim_options));
    return rcpp_result_gen;
END_RCPP
}
// MSARmdl_em
List MSARmdl_em(arma::vec theta_0, List mdl, int k, List optim_options);
RcppExport SEXP _MSTest_MSARmdl_em(SEXP theta_0SEXP, SEXP mdlSEXP, SEXP kSEXP, SEXP optim_optionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta_0(theta_0SEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< List >::type optim_options(optim_optionsSEXP);
    rcpp_result_gen = Rcpp::wrap(MSARmdl_em(theta_0, mdl, k, optim_options));
    return rcpp_result_gen;
END_RCPP
}
// MSVARmdl_em
List MSVARmdl_em(arma::vec theta_0, List mdl, int k, List optim_options);
RcppExport SEXP _MSTest_MSVARmdl_em(SEXP theta_0SEXP, SEXP mdlSEXP, SEXP kSEXP, SEXP optim_optionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta_0(theta_0SEXP);
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< List >::type optim_options(optim_optionsSEXP);
    rcpp_result_gen = Rcpp::wrap(MSVARmdl_em(theta_0, mdl, k, optim_options));
    return rcpp_result_gen;
END_RCPP
}
// calc_mu2t_mv
arma::vec calc_mu2t_mv(List mdl, double rho, List ltmt, arma::vec hv);
RcppExport SEXP _MSTest_calc_mu2t_mv(SEXP mdlSEXP, SEXP rhoSEXP, SEXP ltmtSEXP, SEXP hvSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< List >::type ltmt(ltmtSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type hv(hvSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_mu2t_mv(mdl, rho, ltmt, hv));
    return rcpp_result_gen;
END_RCPP
}
// calc_mu2t
arma::vec calc_mu2t(List mdl, double rho, List ltmt);
RcppExport SEXP _MSTest_calc_mu2t(SEXP mdlSEXP, SEXP rhoSEXP, SEXP ltmtSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< List >::type ltmt(ltmtSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_mu2t(mdl, rho, ltmt));
    return rcpp_result_gen;
END_RCPP
}
// chpStat
arma::vec chpStat(List mdl, double rho_b, List ltmt, int msvar);
RcppExport SEXP _MSTest_chpStat(SEXP mdlSEXP, SEXP rho_bSEXP, SEXP ltmtSEXP, SEXP msvarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< double >::type rho_b(rho_bSEXP);
    Rcpp::traits::input_parameter< List >::type ltmt(ltmtSEXP);
    Rcpp::traits::input_parameter< int >::type msvar(msvarSEXP);
    rcpp_result_gen = Rcpp::wrap(chpStat(mdl, rho_b, ltmt, msvar));
    return rcpp_result_gen;
END_RCPP
}
// bootCV
arma::mat bootCV(List mdl, double rho_b, int N, int msvar);
RcppExport SEXP _MSTest_bootCV(SEXP mdlSEXP, SEXP rho_bSEXP, SEXP NSEXP, SEXP msvarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl(mdlSEXP);
    Rcpp::traits::input_parameter< double >::type rho_b(rho_bSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type msvar(msvarSEXP);
    rcpp_result_gen = Rcpp::wrap(bootCV(mdl, rho_b, N, msvar));
    return rcpp_result_gen;
END_RCPP
}
// simuMdl
List simuMdl(List mdl_h0, int p, int q, int k, int burnin);
RcppExport SEXP _MSTest_simuMdl(SEXP mdl_h0SEXP, SEXP pSEXP, SEXP qSEXP, SEXP kSEXP, SEXP burninSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    rcpp_result_gen = Rcpp::wrap(simuMdl(mdl_h0, p, q, k, burnin));
    return rcpp_result_gen;
END_RCPP
}
// mdledit
List mdledit(List mdl_h0, arma::vec theta_h0, int p, int q, int k0);
RcppExport SEXP _MSTest_mdledit(SEXP mdl_h0SEXP, SEXP theta_h0SEXP, SEXP pSEXP, SEXP qSEXP, SEXP k0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta_h0(theta_h0SEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type k0(k0SEXP);
    rcpp_result_gen = Rcpp::wrap(mdledit(mdl_h0, theta_h0, p, q, k0));
    return rcpp_result_gen;
END_RCPP
}
// compu_tstat
double compu_tstat(arma::vec theta_h0, arma::vec theta_h1, List mdl_h0, List mdl_h1, int p, int q, int k0, int k1);
RcppExport SEXP _MSTest_compu_tstat(SEXP theta_h0SEXP, SEXP theta_h1SEXP, SEXP mdl_h0SEXP, SEXP mdl_h1SEXP, SEXP pSEXP, SEXP qSEXP, SEXP k0SEXP, SEXP k1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta_h0(theta_h0SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta_h1(theta_h1SEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h1(mdl_h1SEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type k0(k0SEXP);
    Rcpp::traits::input_parameter< int >::type k1(k1SEXP);
    rcpp_result_gen = Rcpp::wrap(compu_tstat(theta_h0, theta_h1, mdl_h0, mdl_h1, p, q, k0, k1));
    return rcpp_result_gen;
END_RCPP
}
// LR_samp_dist
arma::vec LR_samp_dist(List mdl_h0, int k1, int N, int burnin, List mdl_h0_control, List mdl_h1_control);
RcppExport SEXP _MSTest_LR_samp_dist(SEXP mdl_h0SEXP, SEXP k1SEXP, SEXP NSEXP, SEXP burninSEXP, SEXP mdl_h0_controlSEXP, SEXP mdl_h1_controlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< int >::type k1(k1SEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h0_control(mdl_h0_controlSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h1_control(mdl_h1_controlSEXP);
    rcpp_result_gen = Rcpp::wrap(LR_samp_dist(mdl_h0, k1, N, burnin, mdl_h0_control, mdl_h1_control));
    return rcpp_result_gen;
END_RCPP
}
// MMCLRpval_fun
double MMCLRpval_fun(arma::vec theta, List mdl_h0, List mdl_h1, int N, int burnin, int workers, double lambda, bool stationary_constraint, double thtol, List mdl_h0_control, List mdl_h1_control);
RcppExport SEXP _MSTest_MMCLRpval_fun(SEXP thetaSEXP, SEXP mdl_h0SEXP, SEXP mdl_h1SEXP, SEXP NSEXP, SEXP burninSEXP, SEXP workersSEXP, SEXP lambdaSEXP, SEXP stationary_constraintSEXP, SEXP thtolSEXP, SEXP mdl_h0_controlSEXP, SEXP mdl_h1_controlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h1(mdl_h1SEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< int >::type workers(workersSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< bool >::type stationary_constraint(stationary_constraintSEXP);
    Rcpp::traits::input_parameter< double >::type thtol(thtolSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h0_control(mdl_h0_controlSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h1_control(mdl_h1_controlSEXP);
    rcpp_result_gen = Rcpp::wrap(MMCLRpval_fun(theta, mdl_h0, mdl_h1, N, burnin, workers, lambda, stationary_constraint, thtol, mdl_h0_control, mdl_h1_control));
    return rcpp_result_gen;
END_RCPP
}
// MMCLRpval_fun_min
double MMCLRpval_fun_min(arma::vec theta, List mdl_h0, List mdl_h1, int N, int burnin, int workers, double lambda, bool stationary_constraint, double thtol, List mdl_h0_control, List mdl_h1_control);
RcppExport SEXP _MSTest_MMCLRpval_fun_min(SEXP thetaSEXP, SEXP mdl_h0SEXP, SEXP mdl_h1SEXP, SEXP NSEXP, SEXP burninSEXP, SEXP workersSEXP, SEXP lambdaSEXP, SEXP stationary_constraintSEXP, SEXP thtolSEXP, SEXP mdl_h0_controlSEXP, SEXP mdl_h1_controlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h0(mdl_h0SEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h1(mdl_h1SEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< int >::type workers(workersSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< bool >::type stationary_constraint(stationary_constraintSEXP);
    Rcpp::traits::input_parameter< double >::type thtol(thtolSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h0_control(mdl_h0_controlSEXP);
    Rcpp::traits::input_parameter< List >::type mdl_h1_control(mdl_h1_controlSEXP);
    rcpp_result_gen = Rcpp::wrap(MMCLRpval_fun_min(theta, mdl_h0, mdl_h1, N, burnin, workers, lambda, stationary_constraint, thtol, mdl_h0_control, mdl_h1_control));
    return rcpp_result_gen;
END_RCPP
}
// calc_DLmoments
arma::vec calc_DLmoments(arma::vec ehat);
RcppExport SEXP _MSTest_calc_DLmoments(SEXP ehatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type ehat(ehatSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_DLmoments(ehat));
    return rcpp_result_gen;
END_RCPP
}
// sim_DLmoments
arma::mat sim_DLmoments(int Tsize, int N);
RcppExport SEXP _MSTest_sim_DLmoments(SEXP TsizeSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type Tsize(TsizeSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(sim_DLmoments(Tsize, N));
    return rcpp_result_gen;
END_RCPP
}
// combine_stat
arma::vec combine_stat(arma::mat stats, arma::mat params, std::string type);
RcppExport SEXP _MSTest_combine_stat(SEXP statsSEXP, SEXP paramsSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type stats(statsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(combine_stat(stats, params, type));
    return rcpp_result_gen;
END_RCPP
}
// approx_dist_loop
arma::mat approx_dist_loop(arma::mat SN2);
RcppExport SEXP _MSTest_approx_dist_loop(SEXP SN2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type SN2(SN2SEXP);
    rcpp_result_gen = Rcpp::wrap(approx_dist_loop(SN2));
    return rcpp_result_gen;
END_RCPP
}
// DLMMCpval_fun
double DLMMCpval_fun(arma::vec theta, arma::vec y, arma::mat x, arma::mat params, arma::vec sim_stats, Rcpp::String pval_type, bool stationary_ind, double lambda);
RcppExport SEXP _MSTest_DLMMCpval_fun(SEXP thetaSEXP, SEXP ySEXP, SEXP xSEXP, SEXP paramsSEXP, SEXP sim_statsSEXP, SEXP pval_typeSEXP, SEXP stationary_indSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sim_stats(sim_statsSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type pval_type(pval_typeSEXP);
    Rcpp::traits::input_parameter< bool >::type stationary_ind(stationary_indSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(DLMMCpval_fun(theta, y, x, params, sim_stats, pval_type, stationary_ind, lambda));
    return rcpp_result_gen;
END_RCPP
}
// DLMMCpval_fun_min
double DLMMCpval_fun_min(arma::vec theta, arma::vec y, arma::mat x, arma::mat params, arma::vec sim_stats, Rcpp::String pval_type, bool stationary_ind, double lambda);
RcppExport SEXP _MSTest_DLMMCpval_fun_min(SEXP thetaSEXP, SEXP ySEXP, SEXP xSEXP, SEXP paramsSEXP, SEXP sim_statsSEXP, SEXP pval_typeSEXP, SEXP stationary_indSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sim_stats(sim_statsSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type pval_type(pval_typeSEXP);
    Rcpp::traits::input_parameter< bool >::type stationary_ind(stationary_indSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(DLMMCpval_fun_min(theta, y, x, params, sim_stats, pval_type, stationary_ind, lambda));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MSTest_cov2corr", (DL_FUNC) &_MSTest_cov2corr, 1},
    {"_MSTest_covar_vech", (DL_FUNC) &_MSTest_covar_vech, 1},
    {"_MSTest_covar_unvech", (DL_FUNC) &_MSTest_covar_unvech, 2},
    {"_MSTest_randP", (DL_FUNC) &_MSTest_randP, 1},
    {"_MSTest_limP", (DL_FUNC) &_MSTest_limP, 1},
    {"_MSTest_ts_lagged", (DL_FUNC) &_MSTest_ts_lagged, 2},
    {"_MSTest_paramList_MSARmdl", (DL_FUNC) &_MSTest_paramList_MSARmdl, 5},
    {"_MSTest_paramList_MSVARmdl", (DL_FUNC) &_MSTest_paramList_MSVARmdl, 6},
    {"_MSTest_calcResid_MSARmdl", (DL_FUNC) &_MSTest_calcResid_MSARmdl, 3},
    {"_MSTest_calcResid_MSVARmdl", (DL_FUNC) &_MSTest_calcResid_MSVARmdl, 3},
    {"_MSTest_initVals_HMmdl", (DL_FUNC) &_MSTest_initVals_HMmdl, 2},
    {"_MSTest_initVals_MSARmdl", (DL_FUNC) &_MSTest_initVals_MSARmdl, 2},
    {"_MSTest_initVals_MSVARmdl", (DL_FUNC) &_MSTest_initVals_MSVARmdl, 2},
    {"_MSTest_MCpval", (DL_FUNC) &_MSTest_MCpval, 3},
    {"_MSTest_randSN", (DL_FUNC) &_MSTest_randSN, 2},
    {"_MSTest_simuAR", (DL_FUNC) &_MSTest_simuAR, 2},
    {"_MSTest_simuMSAR", (DL_FUNC) &_MSTest_simuMSAR, 2},
    {"_MSTest_simuVAR", (DL_FUNC) &_MSTest_simuVAR, 2},
    {"_MSTest_simuMSVAR", (DL_FUNC) &_MSTest_simuMSVAR, 2},
    {"_MSTest_simuNorm", (DL_FUNC) &_MSTest_simuNorm, 2},
    {"_MSTest_simuHMM", (DL_FUNC) &_MSTest_simuHMM, 2},
    {"_MSTest_logLike_Nmdl", (DL_FUNC) &_MSTest_logLike_Nmdl, 2},
    {"_MSTest_logLike_ARmdl", (DL_FUNC) &_MSTest_logLike_ARmdl, 2},
    {"_MSTest_logLike_VARmdl", (DL_FUNC) &_MSTest_logLike_VARmdl, 2},
    {"_MSTest_logLike_HMmdl", (DL_FUNC) &_MSTest_logLike_HMmdl, 3},
    {"_MSTest_logLike_HMmdl_min", (DL_FUNC) &_MSTest_logLike_HMmdl_min, 3},
    {"_MSTest_logLike_MSARmdl", (DL_FUNC) &_MSTest_logLike_MSARmdl, 3},
    {"_MSTest_logLike_MSARmdl_min", (DL_FUNC) &_MSTest_logLike_MSARmdl_min, 3},
    {"_MSTest_logLike_MSVARmdl", (DL_FUNC) &_MSTest_logLike_MSVARmdl, 3},
    {"_MSTest_logLike_MSVARmdl_min", (DL_FUNC) &_MSTest_logLike_MSVARmdl_min, 3},
    {"_MSTest_ExpectationM_HMmdl", (DL_FUNC) &_MSTest_ExpectationM_HMmdl, 3},
    {"_MSTest_ExpectationM_MSARmdl", (DL_FUNC) &_MSTest_ExpectationM_MSARmdl, 3},
    {"_MSTest_ExpectationM_MSVARmdl", (DL_FUNC) &_MSTest_ExpectationM_MSVARmdl, 3},
    {"_MSTest_EMaximization_HMmdl", (DL_FUNC) &_MSTest_EMaximization_HMmdl, 4},
    {"_MSTest_EMaximization_MSARmdl", (DL_FUNC) &_MSTest_EMaximization_MSARmdl, 4},
    {"_MSTest_EMaximization_MSVARmdl", (DL_FUNC) &_MSTest_EMaximization_MSVARmdl, 4},
    {"_MSTest_EMiter_HMmdl", (DL_FUNC) &_MSTest_EMiter_HMmdl, 3},
    {"_MSTest_EMiter_MSARmdl", (DL_FUNC) &_MSTest_EMiter_MSARmdl, 3},
    {"_MSTest_EMiter_MSVARmdl", (DL_FUNC) &_MSTest_EMiter_MSVARmdl, 3},
    {"_MSTest_HMmdl_em", (DL_FUNC) &_MSTest_HMmdl_em, 4},
    {"_MSTest_MSARmdl_em", (DL_FUNC) &_MSTest_MSARmdl_em, 4},
    {"_MSTest_MSVARmdl_em", (DL_FUNC) &_MSTest_MSVARmdl_em, 4},
    {"_MSTest_calc_mu2t_mv", (DL_FUNC) &_MSTest_calc_mu2t_mv, 4},
    {"_MSTest_calc_mu2t", (DL_FUNC) &_MSTest_calc_mu2t, 3},
    {"_MSTest_chpStat", (DL_FUNC) &_MSTest_chpStat, 4},
    {"_MSTest_bootCV", (DL_FUNC) &_MSTest_bootCV, 4},
    {"_MSTest_simuMdl", (DL_FUNC) &_MSTest_simuMdl, 5},
    {"_MSTest_mdledit", (DL_FUNC) &_MSTest_mdledit, 5},
    {"_MSTest_compu_tstat", (DL_FUNC) &_MSTest_compu_tstat, 8},
    {"_MSTest_LR_samp_dist", (DL_FUNC) &_MSTest_LR_samp_dist, 6},
    {"_MSTest_MMCLRpval_fun", (DL_FUNC) &_MSTest_MMCLRpval_fun, 11},
    {"_MSTest_MMCLRpval_fun_min", (DL_FUNC) &_MSTest_MMCLRpval_fun_min, 11},
    {"_MSTest_calc_DLmoments", (DL_FUNC) &_MSTest_calc_DLmoments, 1},
    {"_MSTest_sim_DLmoments", (DL_FUNC) &_MSTest_sim_DLmoments, 2},
    {"_MSTest_combine_stat", (DL_FUNC) &_MSTest_combine_stat, 3},
    {"_MSTest_approx_dist_loop", (DL_FUNC) &_MSTest_approx_dist_loop, 1},
    {"_MSTest_DLMMCpval_fun", (DL_FUNC) &_MSTest_DLMMCpval_fun, 8},
    {"_MSTest_DLMMCpval_fun_min", (DL_FUNC) &_MSTest_DLMMCpval_fun_min, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_MSTest(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}