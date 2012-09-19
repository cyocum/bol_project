#include <iostream>
#include <iomanip>
#include <cstdlib>

#include <mlpack/core.hpp>
#include <mlpack/core/kernels/cosine_distance.hpp>
#include <mlpack/core/data/load.hpp>
#include <mlpack/methods/kmeans/kmeans.hpp>

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
  kmeans.Cluster(data, 80, output);

  std::cout << "Cluster Output" << std::endl;
  std::cout << output << std::endl;
  data::Save("clusters.csv", output, true);

  return EXIT_SUCCESS;
}
