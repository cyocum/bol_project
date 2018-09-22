import Clustering
import Distances
import MultivariateStats
import DelimitedFiles

import Plots
using Distributed

function create_tuple(result, i, data)
    t_name = text_name(data[i,1])
    v_name = book_name(data[i,1])
    c_num = result.assignments[i]

    (c_num, t_name, v_name, is_medoid(i, result))
end

function text_name(text_path) 
    file_name = basename(text_path)
    name = splitext(file_name)[1]
    replace(name, r"_" => s" ")
end

function book_name(text_path)
    dir_name = basename(dirname(text_path))
    replace(dir_name, r"_" => s" ")
end

function is_medoid(i, result)
    i == result.medoids[result.assignments[i]]
end 

(data, header) = DelimitedFiles.readdlm("texts.csv", ',', header=true)
tf_idfs = convert(Array{Float64, 2}, data[:, 2:end])
dists = Distances.pairwise(Distances.CosineDist(), transpose(tf_idfs))

function compute_clusters(k)
    println("k is $k")
    result = Clustering.kmedoids(dists, k)

    clusters = [create_tuple(result, i, data) for i = 1:size(data)[1]]

    sort!(clusters)

    DelimitedFiles.writedlm("clustered-$k.csv", clusters, ',')
    sils = Clustering.silhouettes(result, dists)

    p = Plots.histogram(sils, xlim=(-1.0,1.0), title="Silhouette k = $k")
    Plots.png(p, "sils_$k.png")    
end

for k = 2:164
    compute_clusters(k)
end
