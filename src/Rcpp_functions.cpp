#include <Rcpp.h>
using namespace Rcpp;

//' signCurrent
//'
//' Takes a vector of current values from a data frame of raw data, and assigns a letter
//' ("D", for discharging, "C", for charging or "R" for resting) based on the sign of the current.
//' This function is called as part of the arbin_process_Rmap function.
//' @param x The vector of current values to be processed
//' @export
//' @examples
//' raw$state <- signCurrent(raw$I)
// [[Rcpp::export]]
CharacterVector signCurrent(NumericVector x) {

  int n = x.size();
  CharacterVector out(n);

  for (int i = 0; i < n; ++i) {
    if (x[i] == 0) {
      out[i] = "R";
    } else if (x[i] < 0) {
      out[i] = "D";
    } else if (x[i] > 0) {
      out[i] = "C";
    }
  }
  return out;
}

//' assignRest
//'
//' Takes a vector of character values outputted by the signCurrent function and assigns a unique
//' identifier for each distinct current-rest cycle. Called as part of the arbin_process_Rmap function.
//'
//' @param x The vector outputted by signCurrent to be processed.
//' @export
//' @examples
//' raw$rests <- assignRest(raw$state)
// [[Rcpp::export]]
NumericVector assignRest(CharacterVector x) {

  int n = x.size();
  int countrest = 1;
  NumericVector out(n);

  for (int i = 1; i < n; ++i) {
    if (x[i] != "R" && x[i-1] == "R") {
      countrest++;
      out[i] = countrest;
    } else {
      out[i] = countrest;
    }
  }
  return out;
}
