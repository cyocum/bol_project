import Clustering
import Distance

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

(data, header) = readcsv("texts.csv", has_header=true)
tf_idfs = convert(Array{Float64, 2}, data[:, 2:end])
dists = Distance.pairwise(Distance.CosineDist(), transpose(tf_idfs))

result = Clustering.kmedoids(dists, 20)

clusters = [create_tuple(result, i, data) for i = 1:size(data)[1]]

sort!(clusters)

writecsv("clustered.csv", clusters)
