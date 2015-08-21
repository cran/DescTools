
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/math/common_factor.hpp>


using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
int compute_GCD(int a, int b){
  return
    boost::math::gcd(a, b);
}

// [[Rcpp::export]]
int compute_LCM(int a, int b){
  return
  boost::math::lcm(a, b);
}



// [[Rcpp::export]]
List n_pow_sum(NumericVector x, double meanx) {

  double sum2 = 0.0;
  double sum3 = 0.0;
  double sum4 = 0.0;

  double d = 0.0;
  double d2 = 0.0;

  int n = 0;
  int zn = 0;
  int un = 0;

  NumericVector small_val(5);
  NumericVector small_freq(5);

  NumericVector large_val(5);
  NumericVector large_freq(5);

  n = x.size();

  for (int ii=0; ii < x.size(); ii++) {

    d = x[ii] - meanx;

    d2 = d*d;
    sum2 += d2;
    sum3 += d2*d;
    sum4 += d2*d2;

    // unique values
    if(x[ii] != x[ii+1]) un += 1;
    // zero values
    if(x[ii] == 0) zn += 1 ;

  }


  // the 5 smallest values and their frequencies
  small_val[0] += x[0];
  small_freq[0] += 1;

  int i = 1;
  int j = 0;

  while(j < 5 && i < n) {

    if(x[i] == x[i-1] ){
      small_freq[j] += 1 ;
    } else {
      j += 1 ;
      small_val[j] += x[i] ;
      small_freq[j] += 1 ;
    }
    i += 1 ;
  }

  // the 5 largest values and their frequencies
  large_val[0] += x[n-1];
  large_freq[0] += 1;

  i = n-1;
  j = 0;

  while(j < 5 && i >= 0) {

    if(x[i] == x[i-1] ){
      large_freq[j] += 1 ;
    } else {
      j += 1 ;
      large_val[j] += x[i-1] ;
      large_freq[j] += 1 ;
    }
    i -= 1 ;
  }


  return Rcpp::List::create(Rcpp::Named("sum2", sum2),
                            Rcpp::Named("sum3", sum3),
                            Rcpp::Named("sum4", sum4),
                            Rcpp::Named("zero", zn),
                            Rcpp::Named("unique", un),
                            Rcpp::Named("small_val", small_val),
                            Rcpp::Named("small_freq", small_freq),
                            Rcpp::Named("large_val", large_val),
                            Rcpp::Named("large_freq", large_freq)
  );
}





#include <iostream>
#include <sstream>
#include <bitset>
#include <string>
#include <boost/algorithm/string.hpp>

using namespace std;



// [[Rcpp::export]]
std::vector< std::string > conv_DecToBin(std::vector< int > n )
{
  const unsigned g_unMaxBits = 32;

  int len = n.size();
  std::vector< std::string > out(len);

  for( int i=0; i < len; i++ ) {
      bitset<g_unMaxBits> b(n[i]);
      out[i] = b.to_string();
  }

  return out;

}


