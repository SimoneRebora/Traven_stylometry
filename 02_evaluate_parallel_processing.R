# evaluate_parallel_processing

all_features <- read.csv("analysis_features.csv", stringsAsFactors = F)

n_cores <- as.numeric(all_features$value[all_features$feature == "n_cores"])
n_best_imposters <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "n_best_imposters"], " ")))
MFU_series <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "MFU_series"], " ")))
culling <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "culling"], " ")))
validation_rounds <- as.numeric(all_features$value[all_features$feature == "validation_rounds"])
distances <- unlist(strsplit(all_features$value[all_features$feature == "distances"], " "))
unit <- unlist(strsplit(all_features$value[all_features$feature == "unit"], " "))

methods_combination <- expand.grid(n_best_imposters, MFU_series, culling, distances, unit, stringsAsFactors = FALSE)

colnames(methods_combination) <- c("n_imposters", "MFU", "culling", "distance", "unit")
methods_combination$configuration <- 1:length(methods_combination$n_imposters)

methods_combination <- methods_combination[sample(1:length(methods_combination$n_imposters)),]

write.csv(methods_combination, "Methods_combination_parallel.csv", row.names = F)
cat(n_cores, file = "n_cores.txt")

x <- 1:length(methods_combination$n_imposters)
start_end <- split(x, sort(x%%n_cores))
start_end <- sapply(start_end, function(x) paste(x[1],x[length(x)]))
start_end <- paste(start_end, validation_rounds)

cat(start_end, sep = "\n", file = "parallel_processing_instructions.txt")

