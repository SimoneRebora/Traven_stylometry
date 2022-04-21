# evaluate_parallel_processing

all_features <- read.csv("analysis_features.csv", stringsAsFactors = F)

n_cores <- as.numeric(all_features$value[all_features$feature == "n_cores"])
n_best_imposters <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "n_best_imposters"], " ")))
MFW_series <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "MFW_series"], " ")))
imposters_source <- unlist(strsplit(all_features$value[all_features$feature == "imposters_source"], " "))
culling <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "culling"], " ")))
validation_rounds <- as.numeric(all_features$value[all_features$feature == "validation_rounds"])
distances <- unlist(strsplit(all_features$value[all_features$feature == "distances"], " "))

methods_combination <- expand.grid(imposters_source, n_best_imposters, MFW_series, culling, distances, stringsAsFactors = FALSE)

colnames(methods_combination) <- c("corpus", "n_imposters", "MFW", "culling", "distance")

methods_combination <- methods_combination[sample(1:length(methods_combination$corpus)),]

write.csv(methods_combination, "Methods_combination_parallel.csv")
cat(n_cores, file = "n_cores.txt")

x <- 1:length(methods_combination$corpus)
start_end <- split(x, sort(x%%n_cores))
start_end <- sapply(start_end, function(x) paste(x[1],x[length(x)]))
start_end <- paste(start_end, validation_rounds)

cat(start_end, sep = "\n", file = "parallel_processing_instructions.txt")

unlink("Imposters_test__par*")
unlink("progress_par*")
