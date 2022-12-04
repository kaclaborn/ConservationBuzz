using StringDistances
using DelimitedFiles
using DataFrames

data_nyt, header = readdlm("/home/kclabor1/ConservationBuzz/data/corpora/preprocessed/media/docs_nyt_findDuplicates.csv", ',', header = true);
docs_nyt = DataFrame(data_nyt, vec(header));


function stringDist(datasource, start, stop)
filename = []
check = []
    for i in start:stop
        filename = push!(filename, string("text", datasource[i,1]))
        check = push!(check, findall(datasource[i,2], datasource[:,2], Levenshtein()))
    end
    return filename, check
end
;