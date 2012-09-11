#include <iostream>
#include <iomanip>

#include <mlpack/core.hpp>
#include <mlpack/core/kernels/cosine_distance.hpp>
#include <mlpack/core/data/load.hpp>
#include <mlpack/methods/kmeans/kmeans.hpp>

using namespace std;

using namespace mlpack;
using namespace mlpack::kernel;
using namespace mlpack::metric;
using namespace mlpack::kmeans;
using namespace mlpack::data;

int main(int argc, char **argv) {
  arma::mat data;
  data::Load("texts.csv", data, true);

  arma::Col<size_t> output;

  KMeans<CosineDistance, RandomPartition, MaxVarianceNewCluster> kmeans;
  kmeans.Cluster(data, 20, output);

  cout << "Cluster Output" << endl;
  cout << output << endl;
}
