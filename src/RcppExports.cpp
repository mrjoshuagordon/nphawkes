// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// ComputeDistance
NumericVector ComputeDistance(NumericVector x, NumericVector y, int n);
RcppExport SEXP nphawkes_ComputeDistance(SEXP xSEXP, SEXP ySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeDistance(x, y, n));
    return rcpp_result_gen;
END_RCPP
}
// ComputeTimeDifference
NumericVector ComputeTimeDifference(NumericVector x);
RcppExport SEXP nphawkes_ComputeTimeDifference(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeTimeDifference(x));
    return rcpp_result_gen;
END_RCPP
}
// ComputeTriggeringMarkedSpatialTemporal
List ComputeTriggeringMarkedSpatialTemporal(NumericVector g, NumericVector f, NumericVector kp, double mu, int n);
RcppExport SEXP nphawkes_ComputeTriggeringMarkedSpatialTemporal(SEXP gSEXP, SEXP fSEXP, SEXP kpSEXP, SEXP muSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f(fSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type kp(kpSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeTriggeringMarkedSpatialTemporal(g, f, kp, mu, n));
    return rcpp_result_gen;
END_RCPP
}
// ComputeTriggeringMarkedSpatialTemporalNH
List ComputeTriggeringMarkedSpatialTemporalNH(NumericVector g, NumericVector f, NumericVector kp, NumericVector mu, int n);
RcppExport SEXP nphawkes_ComputeTriggeringMarkedSpatialTemporalNH(SEXP gSEXP, SEXP fSEXP, SEXP kpSEXP, SEXP muSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f(fSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type kp(kpSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeTriggeringMarkedSpatialTemporalNH(g, f, kp, mu, n));
    return rcpp_result_gen;
END_RCPP
}
// ComputeTriggeringSpatialTemporal
List ComputeTriggeringSpatialTemporal(NumericVector g, NumericVector f, double mu, int n);
RcppExport SEXP nphawkes_ComputeTriggeringSpatialTemporal(SEXP gSEXP, SEXP fSEXP, SEXP muSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type f(fSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeTriggeringSpatialTemporal(g, f, mu, n));
    return rcpp_result_gen;
END_RCPP
}
// ComputeTriggeringTemporal
List ComputeTriggeringTemporal(NumericVector g, double mu, int n);
RcppExport SEXP nphawkes_ComputeTriggeringTemporal(SEXP gSEXP, SEXP muSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeTriggeringTemporal(g, mu, n));
    return rcpp_result_gen;
END_RCPP
}
// ComputeTriggeringTemporalNS
List ComputeTriggeringTemporalNS(NumericVector g, double kp, NumericVector mu, int n);
RcppExport SEXP nphawkes_ComputeTriggeringTemporalNS(SEXP gSEXP, SEXP kpSEXP, SEXP muSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< double >::type kp(kpSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ComputeTriggeringTemporalNS(g, kp, mu, n));
    return rcpp_result_gen;
END_RCPP
}
// distanceFrom
NumericVector distanceFrom(NumericVector x1, NumericVector y1, int j);
RcppExport SEXP nphawkes_distanceFrom(SEXP x1SEXP, SEXP y1SEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(distanceFrom(x1, y1, j));
    return rcpp_result_gen;
END_RCPP
}
// distanceFrom2
NumericVector distanceFrom2(double x0, double y0, NumericVector x1, NumericVector y1);
RcppExport SEXP nphawkes_distanceFrom2(SEXP x0SEXP, SEXP y0SEXP, SEXP x1SEXP, SEXP y1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x0(x0SEXP);
    Rcpp::traits::input_parameter< double >::type y0(y0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    rcpp_result_gen = Rcpp::wrap(distanceFrom2(x0, y0, x1, y1));
    return rcpp_result_gen;
END_RCPP
}
// getBackgroudPosition
List getBackgroudPosition(int nbins_x, int nbins_y, NumericVector x, NumericVector y, NumericVector grid_x, NumericVector grid_y, int n);
RcppExport SEXP nphawkes_getBackgroudPosition(SEXP nbins_xSEXP, SEXP nbins_ySEXP, SEXP xSEXP, SEXP ySEXP, SEXP grid_xSEXP, SEXP grid_ySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nbins_x(nbins_xSEXP);
    Rcpp::traits::input_parameter< int >::type nbins_y(nbins_ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_x(grid_xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_y(grid_ySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(getBackgroudPosition(nbins_x, nbins_y, x, y, grid_x, grid_y, n));
    return rcpp_result_gen;
END_RCPP
}
// getBackground
NumericVector getBackground(NumericVector x, NumericVector y, double t, NumericVector pbNew, NumericVector nn, int nbins_x, int nbins_y, NumericVector grid_x, NumericVector grid_y, double delta_x, double delta_y, int n);
RcppExport SEXP nphawkes_getBackground(SEXP xSEXP, SEXP ySEXP, SEXP tSEXP, SEXP pbNewSEXP, SEXP nnSEXP, SEXP nbins_xSEXP, SEXP nbins_ySEXP, SEXP grid_xSEXP, SEXP grid_ySEXP, SEXP delta_xSEXP, SEXP delta_ySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pbNew(pbNewSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type nn(nnSEXP);
    Rcpp::traits::input_parameter< int >::type nbins_x(nbins_xSEXP);
    Rcpp::traits::input_parameter< int >::type nbins_y(nbins_ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_x(grid_xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_y(grid_ySEXP);
    Rcpp::traits::input_parameter< double >::type delta_x(delta_xSEXP);
    Rcpp::traits::input_parameter< double >::type delta_y(delta_ySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(getBackground(x, y, t, pbNew, nn, nbins_x, nbins_y, grid_x, grid_y, delta_x, delta_y, n));
    return rcpp_result_gen;
END_RCPP
}
// getBackgroundT
NumericVector getBackgroundT(NumericVector t, NumericVector pbNew, NumericVector nn, int nbins_mu, NumericVector grid_mu, double delta_mu, int n);
RcppExport SEXP nphawkes_getBackgroundT(SEXP tSEXP, SEXP pbNewSEXP, SEXP nnSEXP, SEXP nbins_muSEXP, SEXP grid_muSEXP, SEXP delta_muSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pbNew(pbNewSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type nn(nnSEXP);
    Rcpp::traits::input_parameter< int >::type nbins_mu(nbins_muSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_mu(grid_muSEXP);
    Rcpp::traits::input_parameter< double >::type delta_mu(delta_muSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(getBackgroundT(t, pbNew, nn, nbins_mu, grid_mu, delta_mu, n));
    return rcpp_result_gen;
END_RCPP
}
// getDistanceBins
NumericVector getDistanceBins(int nbins_r, NumericVector dist, NumericVector grid_r);
RcppExport SEXP nphawkes_getDistanceBins(SEXP nbins_rSEXP, SEXP distSEXP, SEXP grid_rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nbins_r(nbins_rSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dist(distSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_r(grid_rSEXP);
    rcpp_result_gen = Rcpp::wrap(getDistanceBins(nbins_r, dist, grid_r));
    return rcpp_result_gen;
END_RCPP
}
// getMagnitudeBins
NumericVector getMagnitudeBins(int nbins_m, NumericVector mags, NumericVector grid_m);
RcppExport SEXP nphawkes_getMagnitudeBins(SEXP nbins_mSEXP, SEXP magsSEXP, SEXP grid_mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nbins_m(nbins_mSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mags(magsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_m(grid_mSEXP);
    rcpp_result_gen = Rcpp::wrap(getMagnitudeBins(nbins_m, mags, grid_m));
    return rcpp_result_gen;
END_RCPP
}
// getMags
NumericVector getMags(NumericVector m, int n);
RcppExport SEXP nphawkes_getMags(SEXP mSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(getMags(m, n));
    return rcpp_result_gen;
END_RCPP
}
// getNN
NumericVector getNN(NumericVector x, NumericVector y, int np, int n);
RcppExport SEXP nphawkes_getNN(SEXP xSEXP, SEXP ySEXP, SEXP npSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type np(npSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(getNN(x, y, np, n));
    return rcpp_result_gen;
END_RCPP
}
// getTimeBins
NumericVector getTimeBins(int nbins_t, NumericVector time, NumericVector grid_t);
RcppExport SEXP nphawkes_getTimeBins(SEXP nbins_tSEXP, SEXP timeSEXP, SEXP grid_tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nbins_t(nbins_tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type time(timeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type grid_t(grid_tSEXP);
    rcpp_result_gen = Rcpp::wrap(getTimeBins(nbins_t, time, grid_t));
    return rcpp_result_gen;
END_RCPP
}
// initProb
List initProb(int n);
RcppExport SEXP nphawkes_initProb(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(initProb(n));
    return rcpp_result_gen;
END_RCPP
}
// pKern
double pKern(double prob, double x, double y, double bwd);
RcppExport SEXP nphawkes_pKern(SEXP probSEXP, SEXP xSEXP, SEXP ySEXP, SEXP bwdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type prob(probSEXP);
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type bwd(bwdSEXP);
    rcpp_result_gen = Rcpp::wrap(pKern(prob, x, y, bwd));
    return rcpp_result_gen;
END_RCPP
}
// pKernT
double pKernT(double prob, double t, double bwd);
RcppExport SEXP nphawkes_pKernT(SEXP probSEXP, SEXP tSEXP, SEXP bwdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type prob(probSEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    Rcpp::traits::input_parameter< double >::type bwd(bwdSEXP);
    rcpp_result_gen = Rcpp::wrap(pKernT(prob, t, bwd));
    return rcpp_result_gen;
END_RCPP
}
// replaceMu
NumericVector replaceMu(NumericVector mu, std::vector<std::vector<int> > D, int n);
RcppExport SEXP nphawkes_replaceMu(SEXP muSEXP, SEXP DSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type D(DSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(replaceMu(mu, D, n));
    return rcpp_result_gen;
END_RCPP
}
// sumProbs
bool sumProbs(NumericVector vt, NumericVector vb, int sz);
RcppExport SEXP nphawkes_sumProbs(SEXP vtSEXP, SEXP vbSEXP, SEXP szSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type vt(vtSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vb(vbSEXP);
    Rcpp::traits::input_parameter< int >::type sz(szSEXP);
    rcpp_result_gen = Rcpp::wrap(sumProbs(vt, vb, sz));
    return rcpp_result_gen;
END_RCPP
}
// sumVector
double sumVector(NumericVector v);
RcppExport SEXP nphawkes_sumVector(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(sumVector(v));
    return rcpp_result_gen;
END_RCPP
}
