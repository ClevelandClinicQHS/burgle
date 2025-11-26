#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int sample_time2(NumericVector Haz){
  NumericVector p(Haz.size());
  p = exp(-NumericVector(Haz.begin(), Haz.end()));
  NumericVector p1(Haz.size()+1);

  for(int i = 0; i < Haz.size(); ++i){
    if(i == 0){
      p1[i] = (1-p[i]);


    }else p1[i] = (1-p[i]) - (1-p[i-1]);
  }
  p1[p1.size()-1] = p[p.size()-1];

  int time_idx = Rcpp::sample(Haz.size()+1, 1, false, p1)[0] - 1;
  return(time_idx);
}

// trying something out
// [[Rcpp::export]]
DataFrame get_time_event2(List chst) {
  // int n = trans.size();
  int n0 = chst.size();
  List x = chst[0];
  NumericVector v1 = x["time"];
  // NumericVector v1 = chst[0]["time"];
  int n2 = v1.size();
  int n = n0 * n2;

  CharacterVector trans(n);
  NumericVector time(n);
  NumericVector Haz(n);

  for (int i = 0; i < n0; ++i) {
    for (int j = 0; j < n2; ++j){
      List y = chst[i];
      NumericVector t1 = y["time"];
      NumericVector h1 = y["Haz"];

      trans[i * n2 + j ] = std::to_string(i + 1);
      time[i * n2 + j ]  = t1[j];
      Haz[i * n2 + j ]= h1[j];
    }
  }


  // Step 1: Aggregate hazard by time and store hazard per (time, trans)
  std::map<double, double> hazsum_by_time;
  std::vector<std::string> trans_str(n);
  for (int i = 0; i < n; ++i) {
    hazsum_by_time[time[i]] += Haz[i];
    trans_str[i] = as<std::string>(trans[i]);
  }

  // Step 2: Prepare time sampling
  std::vector<double> unique_times;
  std::vector<double> hazsums;
  for (auto it = hazsum_by_time.begin(); it != hazsum_by_time.end(); ++it) {
    unique_times.push_back(it->first);
    hazsums.push_back(it->second);
  }

  // Safety check: ensure valid time sampling
  double total_time_weight = std::accumulate(hazsums.begin(), hazsums.end(), 0.0);
  if (unique_times.empty() || total_time_weight <= 0.0) {
    stop("No valid time points to sample from.");
  }


  // Step 3: Compute cumulative hazard (HAZSUM1)
  std::vector<double> HAZSUM1(hazsums.size());
  std::partial_sum(hazsums.begin(), hazsums.end(), HAZSUM1.begin());

  // return DataFrame::create(Named("Hazsum") = wrap(hazsums));
  //
  // return DataFrame::create(Named("Hazsum") = wrap(exp(-NumericVector(HAZSUM1.begin(), HAZSUM1.end()))));
  int time_idx = sample_time2(wrap(HAZSUM1));

  // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(exp(-NumericVector(HAZSUM1.begin(), HAZSUM1.end()))))[0] - 1;

  if(time_idx == unique_times.size()) return DataFrame::create(Named("time") = R_NaN, Named("event") = "NaN");


  // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(HAZSUM1))[0] - 1;

  // std::vector<double> weights(HAZSUM1.size());
  //
  // std::transform(HAZSUM1.begin(), HAZSUM1.end(), weights.begin(),{ return std::exp(-x); }
  //
  //
  // // Use weights in Rcpp::sample
  // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(weights))[0] - 1;

  // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(weights))[0] - 1;

  double st = unique_times[time_idx];

  // Step 4: Collect hazard values at sampled time
  std::vector<std::string> trans_at_st;
  NumericVector haz_at_st;
  for (int i = 0; i < n; ++i) {
    if (time[i] == st) {
      trans_at_st.push_back(trans_str[i]);
      haz_at_st.push_back(Haz[i]);
    }
  }

  // Safety check: ensure valid event sampling
  double total_event_weight = std::accumulate(haz_at_st.begin(), haz_at_st.end(), 0.0);
  if (trans_at_st.empty() || total_event_weight <= 0.0) {
    stop("No valid hazard weights to sample event.");
  }

  int event_idx = Rcpp::sample(trans_at_st.size(), 1, false, wrap(haz_at_st))[0] - 1;
  std::string se = trans_at_st[event_idx];

  return DataFrame::create(Named("time") = st, Named("event") = se);

}


// DataFrame get_time_event(CharacterVector trans, NumericVector time, NumericVector Haz) {
//   int n = trans.size();
//
//   // Step 1: Aggregate hazard by time and store hazard per (time, trans)
//   std::map<double, double> hazsum_by_time;
//   std::vector<std::string> trans_str(n);
//   for (int i = 0; i < n; ++i) {
//     hazsum_by_time[time[i]] += Haz[i];
//     trans_str[i] = as<std::string>(trans[i]);
//   }
//
//   // Step 2: Prepare time sampling
//   std::vector<double> unique_times;
//   std::vector<double> hazsums;
//   for (auto it = hazsum_by_time.begin(); it != hazsum_by_time.end(); ++it) {
//     unique_times.push_back(it->first);
//     hazsums.push_back(it->second);
//   }
//
//   // Safety check: ensure valid time sampling
//   double total_time_weight = std::accumulate(hazsums.begin(), hazsums.end(), 0.0);
//   if (unique_times.empty() || total_time_weight <= 0.0) {
//     stop("No valid time points to sample from.");
//   }
//
//
//   // Step 3: Compute cumulative hazard (HAZSUM1)
//   std::vector<double> HAZSUM1(hazsums.size());
//   std::partial_sum(hazsums.begin(), hazsums.end(), HAZSUM1.begin());
//
//   // return DataFrame::create(Named("Hazsum") = wrap(hazsums));
//   //
//   // return DataFrame::create(Named("Hazsum") = wrap(exp(-NumericVector(HAZSUM1.begin(), HAZSUM1.end()))));
//   int time_idx = sample_time2(wrap(HAZSUM1));
//
//   // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(exp(-NumericVector(HAZSUM1.begin(), HAZSUM1.end()))))[0] - 1;
//
//   if(time_idx == unique_times.size()) return DataFrame::create(Named("time") = R_NaN, Named("event") = "NaN");
//
//
//   // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(HAZSUM1))[0] - 1;
//
//   // std::vector<double> weights(HAZSUM1.size());
//   //
//   // std::transform(HAZSUM1.begin(), HAZSUM1.end(), weights.begin(),{ return std::exp(-x); }
//   //
//   //
//   // // Use weights in Rcpp::sample
//   // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(weights))[0] - 1;
//
//   // int time_idx = Rcpp::sample(unique_times.size(), 1, false, wrap(weights))[0] - 1;
//
//   double st = unique_times[time_idx];
//
//   // Step 4: Collect hazard values at sampled time
//   std::vector<std::string> trans_at_st;
//   NumericVector haz_at_st;
//   for (int i = 0; i < n; ++i) {
//     if (time[i] == st) {
//       trans_at_st.push_back(trans_str[i]);
//       haz_at_st.push_back(Haz[i]);
//     }
//   }
//
//   // Safety check: ensure valid event sampling
//   double total_event_weight = std::accumulate(haz_at_st.begin(), haz_at_st.end(), 0.0);
//   if (trans_at_st.empty() || total_event_weight <= 0.0) {
//     stop("No valid hazard weights to sample event.");
//   }
//
//   int event_idx = Rcpp::sample(trans_at_st.size(), 1, false, wrap(haz_at_st))[0] - 1;
//   std::string se = trans_at_st[event_idx];
//
//   return DataFrame::create(Named("time") = st, Named("event") = se);
//
// }

// [[Rcpp::export]]
DataFrame sample_time_event(List chst) {
  std::vector<DataFrame> results;
  Environment pkg = Environment::namespace_env("dplyr");
  Function bind_rows = pkg["bind_rows"];
  // Function bind_rows("bind_rows");

  for (int i = 0; i < chst.size(); ++i) {
    List x = chst[i];

    // CharacterVector trans = x["trans"];
    // NumericVector time = x["time"];
    // NumericVector Haz = x["Haz"];

    // Get_time_event is defined above
    // DataFrame result = get_time_event(trans, time, Haz);
    DataFrame result = get_time_event2(x);
    results.push_back(result);
  }

  // Combine all DataFrames
  return bind_rows(results);


}

// [[Rcpp::export]]
List combine_hazards(List chst) {
  int n = chst.size();
  List result(n);

  Function bind_rows("bind_rows");

  for (int i = 0; i < n; ++i) {
    List sublist = chst[i];
    int m = sublist.size();

    List dfs(m);

    for (int j = 0; j < m; ++j) {
      DataFrame df = as<DataFrame>(sublist[j]);
      CharacterVector trans(df.nrows(), std::to_string(j + 1));
      df["trans"] = trans;
      dfs[j] = df;
    }

    result[i] = bind_rows(dfs);
  }

  return result;
}


// [[Rcpp::export]]
List update_hazards(NumericMatrix lps, List chs) {
  int n_rows = lps.nrow();
  int n_cols = lps.ncol();

  List result(n_rows);

  for (int i = 0; i < n_rows; ++i) {
    List updated_chs(n_cols);

    for (int j = 0; j < n_cols; ++j) {
      double y = lps(i, j);
      DataFrame z = as<DataFrame>(chs[j]);

      NumericVector Haz = z["Haz"];
      NumericVector new_Haz = Haz * std::exp(y);

      // Create updated DataFrame
      DataFrame updated = clone(z);
      updated["Haz"] = new_Haz;

      updated_chs[j] = updated;
    }

    result[i] = updated_chs;
  }

  return result;
}
