import Clustering
import Distance

(data, header) = readcsv("texts.csv", has_header=true)
tf_idfs = transpose(convert(Array{Float64, 2}, data[:, 2:end]))
dists = Distance.pairwise(Distance.CosineDist(), tf_idfs)

result = Clustering.kmedoids(dists, 20)

clusters = [(result.assignments[idx], name)
            for (idx, name) in enumerate(data[:, 1])]

sort!(clusters)
println(clusters)
