#include <Rcpp.h>
#include <RcppEigen.h>
using namespace Rcpp;

// I'll fully acknowledge AI/copilot helped me write this code

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
Eigen::MatrixXd fastmm(const Eigen::MatrixXd& A, const Eigen::MatrixXd& B) {
  if (A.cols() != B.rows()) stop("Incompatible matrix dimensions");
  return A * B;
}

// [[Rcpp::export]]
List simulate_responses(NumericMatrix preds, int sims, bool se, NumericVector rss) {
  int n = preds.nrow();
  int p = preds.ncol();
  NumericVector sd = rss;

  List result(sims);
  NumericMatrix sim(n, p);

  if(se){
    for (int s = 0; s < sims; ++s) {
      for (int j = 0; j < p; ++j) {
        for (int i = 0; i < n; ++i) {

          sim(i, j) = R::rnorm(preds(i, j), sd(i));
        }
      }
      result[s] = clone(sim);  // clone to avoid overwriting
    }
  }else{
    for (int s = 0; s < sims; ++s) {
      sim = preds;
      result[s] = clone(sim);
    }
  }

  return result;
}


// Sample from truncated normal
double rsamp_rnorm(double mean, double sd, double lower, double upper) {
  double val;
  do {
    val = R::rnorm(mean, sd);
  } while (val < lower || val > upper);
  return val;
}

// [[Rcpp::export]]
List simulate_responses_limits(NumericMatrix preds, int sims, bool se, double rss, NumericVector limits) {
  int n = preds.nrow();
  int p = preds.ncol();
  double sd = se ? sqrt(rss) : 0.0;

  List result(sims);
  NumericMatrix sim(n, p);

  for (int s = 0; s < sims; ++s) {
    for (int j = 0; j < p; ++j) {
      for (int i = 0; i < n; ++i) {
        double mean = preds(i, j);
        sim(i, j) = (sd > 0.0) ? rsamp_rnorm(mean, sd, limits[0], limits[1]) : mean;
      }
    }
    result[s] = clone(sim);
  }

  return result;
}

// [[Rcpp::export]]
List simulate_responses_binom(NumericMatrix preds, int sims){

  int n = preds.nrow();
  int p = preds.ncol();

  List result(sims);

  // List result(sims);
  NumericMatrix sim(n, p);

  for (int s = 0; s < sims; ++s) {
    for (int j = 0; j < p; ++j) {
      for (int i = 0; i < n; ++i) {
        // preds
        sim(i, j) = R::rbinom(1, preds(i, j));
      }
    }
    result[s] = clone(sim);  // clone to avoid overwriting
  }

  return result;

}
