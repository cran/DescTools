
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


  // x must be a nonempty sorted numeric vector with NAs omitted

  double d = x[0] - meanx;
  double d2 = d*d;

  double sum2 = d2;
  double sum3 = d2*d;
  double sum4 = d2*d2;

  int n = x.size();    // the vector size
  int zn = 0;
  if(x[0]==0) {zn=1;}; // the number of zeros
  int un = 1;          // the number of unique values

  for (int ii=1; ii < x.size(); ii++) {

    d = x[ii] - meanx;

    d2 = d*d;
    sum2 += d2;     // sum of squares
    sum3 += d2*d;   // sum of 3th powers
    sum4 += d2*d2;  // sum of 4th powers

    // number of unique values
    if(x[ii] != x[ii-1]) un += 1;

    // number of zero values
    if(x[ii] == 0) zn += 1 ;

  }


  int ldim = 5;    // dimension of small/large values vectors
  if(un < ldim) { ldim = un; }

  NumericVector small_val(ldim);    // the 5 smallest values
  NumericVector small_freq(ldim);   // the frequency of the 5 smallest values


  // the 5 smallest values and their frequencies

  small_val[0] = x[0];
  small_freq[0] = 1;

  int i = 1;
  int j = 0;

  do {
    if(x[i] == x[i-1] ){
      small_freq[j] += 1 ;
    } else {
// we have reached the max number of interesting values, so we break
      if(j==ldim-1) break;

      j += 1 ;
      small_val[j] = x[i] ;
      small_freq[j] = 1 ;
    }
    i += 1 ;

  } while(i < n);


  // the 5 largest values and their frequencies

  NumericVector large_val(ldim);
  NumericVector large_freq(ldim);

  large_val[0] = x[n-1];
  large_freq[0] = 1;

  i = n-1;
  j = 0;

  do {

    if(x[i] == x[i-1] ){
      large_freq[j] += 1 ;
    } else {
      // we have reached the max number of interesting values, so we break
      if(j==ldim-1) break;

      j += 1 ;
      large_val[j] = x[i-1] ;
      large_freq[j] = 1 ;
    }
    i -= 1 ;

  } while(i >= 1);


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


