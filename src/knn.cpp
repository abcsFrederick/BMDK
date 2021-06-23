
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//' Calculate k nearest neighbors
//' 
//' @param nrow Number of rows
//' @param ncol Number of columns
//' @param f Features matrix
//'
//' @export
// [[Rcpp::export]]
List knn(int nrow, int ncol, NumericVector f) {
  
  int numFeat= nrow;
  int numSamp= ncol;
  
  NumericVector nnDist(numSamp);
  NumericVector nnIdx(numSamp);
  
  double inf = numeric_limits<double>::infinity();
  
  // Initializations
  NumericVector neighborsDist(numSamp);
  double cumSum;
  double absDiff;
  double minVal;
  int minIdx;
  
  
  // For each sample of interest...
  for (int i = 0; i < numSamp; i++) {
    
    // Initialize neighborsDist to store the distance from sample i and its neighbors. Fill neighborsDist with infinity.
    fill(neighborsDist.begin(), neighborsDist.end(), inf);
    
    // ...iterate through all of the other samples and calculate the Manhattan distance.
    for (int j = 0; j < numSamp; j++) {
      
      // If sample i is not the same sample as sample j (If their start indices are not equal)
      if (j != i) {
        
        // Initialize the cumulative sum value
        cumSum= 0;
        
        // Iterate through each feature value of the samples
        for (int k = 0; k < numFeat; k++) {
          
          absDiff= abs( f[i*numFeat + k] - f[j*numFeat + k] ); // Is this offset correct???
          // Add to the cumulative sum value
          cumSum= cumSum + absDiff;
        }
        
        // Set the jth element equal to the sum of the abs difference between two samples
        neighborsDist[j] =  cumSum;
      }
      
    }
    
    minVal= neighborsDist[0];
    minIdx= 0;
    
    // Find the nearest neighbor of sample i and record the distance and index of that neighbor
    for (int m= 1; m < numSamp; m++) {
      
      if (minVal > neighborsDist[m]) {
        minVal= neighborsDist[m];
        minIdx= m;
      }
    }
    
    nnDist[i] = minVal;
    nnIdx[i] = minIdx;
  }
  
  return List::create(nnDist, nnIdx);
}
