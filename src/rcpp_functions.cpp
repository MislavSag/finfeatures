#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double sum_of_resids_cpp(NumericVector x) {
  int n = x.size();
  double m = (x[n - 1] - x[0]) / (n - 1);
  double resids = 0;
  for (int i = 0; i < n; i++) {
    resids += std::abs(m * i + x[0] - x[i]);
  }
  return resids;
}

// [[Rcpp::export]]
double r_squared_cpp(NumericVector x) {
  int n = x.size();
  double m = (x[n - 1] - x[0]) / (n - 1);
  double x_mean = Rcpp::mean(x);
  double resids = 0;
  double tss = 0;
  for (int i = 0; i < n; i++) {
    resids += std::pow(m * i + x[0] - x[i], 2);
    tss += std::pow(x[i] - x_mean, 2);
  }
  return std::max(0.0, 1 - resids / tss);
}
