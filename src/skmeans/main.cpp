#include <iostream>
#include <iomanip>

#include <mlpack/core.hpp>
#include <mlpack/core/kernels/cosine_distance.hpp>
#include <mlpack/methods/kmeans/kmeans.hpp>

using namespace std;

using namespace mlpack;
using namespace mlpack::kernel;
using namespace mlpack::metric;
using namespace mlpack::kmeans;



int main(int argc, char **argv) {
  KMeans<CosineDistance, RandomPartition, MaxVarianceNewCluster> kmeans;
  cout << "blah" << endl;
}
