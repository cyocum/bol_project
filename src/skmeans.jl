import Clustering
import Distances
import MultivariateStats
using Gadfly

function create_tuple(result, i, data)
    t_name = text_name(data[i,1])
    v_name = book_name(data[i,1])
    c_num = result.assignments[i]

    (c_num, t_name, v_name, is_medoid(i, result))
end

function text_name(text_path) 
    file_name = basename(text_path)
    name = splitext(file_name)[1]
    replace(name, "_", " ")
end

function book_name(text_path)
    dir_name = basename(dirname(text_path))
    replace(dir_name, "_", " ")
end

function is_medoid(i, result)
    i == result.medoids[result.assignments[i]]
end 

(data, header) = readcsv("texts.csv", header=true)
tf_idfs = convert(Array{Float64, 2}, data[:, 2:end])
#dists = Distances.pairwise(Distances.CosineDist(), transpose(tf_idfs))

#result = Clustering.kmedoids(dists, 9)

#clusters = [create_tuple(result, i, data) for i = 1:size(data)[1]]

#sort!(clusters)

#writecsv("clustered-test-9.csv", clusters)
#sils = Clustering.silhouettes(result, dists)
#p=plot(x=sils, Geom.histogram, Theme(background_color=color("white"), panel_opacity=0))
#draw(PNG("sils-20.png", 24cm, 12cm), p)

vectors = transpose(tf_idfs)
pca = MultivariateStats.fit(MultivariateStats.PCA, vectors)
pca_plot = Gadfly.plot(x=pca.proj[:,1], y=pca.proj[:,2], Geom.point, Theme(background_color=color("white"), panel_opacity=0))
draw(PNG("pca-new.svg", 24cm, 12cm), pca_plot)
