#include <Rcpp.h>
#include <RcppEigen.h>
using namespace Rcpp;

// I'll fully acknowledge AI helped me write this code

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
Eigen::MatrixXd fastmm(const Eigen::MatrixXd& A, const Eigen::MatrixXd& B) {
  if (A.cols() != B.rows()) stop("Incompatible matrix dimensions");
  return A * B;
}

// [[Rcpp::export]]
List simulate_responses(NumericMatrix preds, int sims, bool se, double rss) {
  int n = preds.nrow();
  int p = preds.ncol();
  double sd = se ? std::sqrt(rss) : 0.0;

  List result(sims);
  NumericMatrix sim(n, p);

  for (int s = 0; s < sims; ++s) {
    for (int j = 0; j < p; ++j) {
      for (int i = 0; i < n; ++i) {
        sim(i, j) = sd > 0.0 ? R::rnorm(preds(i, j), sd) : preds(i, j);
      }
    }
    result[s] = clone(sim);  // clone to avoid overwriting
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
  double sd = se ? std::sqrt(rss) : 0.0;

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
  // int m.size();
  // NumericMatrix preds = m[1];
  int n = preds.nrow();
  int p = preds.ncol();

  // return(p);
  List result(sims);
  // return(result);

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



// if(!is.null(dim(preds))){
//   pn <- apply(preds, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x))
// }else{
//   pn <- lapply(preds,
//                function(y) apply(y, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)))
// }
