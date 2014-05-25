import Clustering
import Distance

(data, header) = readcsv("texts.csv", has_header=true)
tf_idfs = convert(Array{Float64, 2}, data[:, 2:end])
dists = Distance.pairwise(Distance.CosineDist(), transpose(tf_idfs))

@profile result = Clustering.kmedoids(dists, 20)
