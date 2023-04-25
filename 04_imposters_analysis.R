### Imposters_analysis

library(stylo)

methods_combination <- read.csv("Methods_combination_parallel.csv", stringsAsFactors = F)

load(paste("Preprocessed_dataset_", methods_combination$unit[1], ".RData", sep = ""))

args <- commandArgs(trailingOnly=TRUE)
args_split <- unlist(strsplit(args, " "))

start_loop <- as.numeric(args_split[1])
end_loop <- as.numeric(args_split[2])
validation_rounds <- as.numeric(args_split[3])

filename <- paste("Final_results_", methods_combination$unit[1], "_", start_loop, ".csv", sep = "")

### 0. Functions

# new minmax function
dist.minmaxfast = function(x){
  
  # test if the input dataset is acceptable
  if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
    stop("cannot apply a distance measure: wrong data format!")
  }
  # then, test whether the number of rows and cols is >1
  if(length(x[1,]) < 2 | length(x[,1]) < 2) {
    stop("at least 2 cols and 2 rows are needed to compute a distance!")
  }
  
  # getting the size of the input table
  rows = length(x[,1])
  # starting a new matrix
  y = matrix(nrow = rows, ncol = rows)
  rownames(y) = rownames(x)
  colnames(y) = rownames(x)
  # iterating over rows and columns
  for(i in 1:rows) {
    for(j in i:rows ) {
      y[j,i] = 1 - sum(pmin(x[i,], x[j,])) / sum(pmax(x[i,], x[j,]))
    }
  }
  
  # converting the matrix to the class 'dist'
  y = as.dist(y)
  
  return(y)
}

### 1. First phase: testing

full_results <- data.frame()

for(method in start_loop:end_loop){
  
  # cat("\n######\n######\n###### Configuration", method, "\n\n")
  
  # cat("\n######\n######\n Right author\n\n")
  
  for(candidate_id in unique(full_corpus_metadata$candidate_id)){
    
    # cat("\n######\n no.", candidate_id, "\n\n")
    
    available_corpora <- which(full_corpus_metadata$analysis_type == "verifications" &
                                 full_corpus_metadata$impostors == methods_combination$n_imposters[method] &
                                 full_corpus_metadata$candidate_id == candidate_id)
    
    for(validation in 1:validation_rounds){
      
      # select random text
      selected_id <- sample(available_corpora, 1)
      
      # load preprocessed dataset 
      data <- full_corpus_list[[selected_id]]
      
      # culling
      data <- perform.culling(data, culling.level = methods_combination$culling[method])
      if(dim(data)[2] > methods_combination$MFU[method])
        data <- data[,1:methods_combination$MFU[method]]
      
      # apply the imposters
      imposters_results <- imposters(reference.set = data[-c(1,2),], test = data[1,], candidate.set = data[2,], distance = methods_combination$distance[method])
      
      # save incrementally
      full_results <- rbind(full_results, data.frame(configuration = methods_combination$configuration[method], repetition = ((candidate_id-1)*validation_rounds)+validation, analysis_type = "green", value = imposters_results))
      
    }
    
  }
  
  # cat("\n######\n######\n Wrong author\n\n")
  
  for(candidate_id in unique(full_corpus_metadata$candidate_id)){
    
    # cat("\n######\n no.", candidate_id, "\n\n")
    
    available_corpora <- which(full_corpus_metadata$analysis_type == "convalidations" &
                                 full_corpus_metadata$impostors == methods_combination$n_imposters[method] &
                                 full_corpus_metadata$candidate_id == candidate_id)
    
    for(validation in 1:validation_rounds){
      
      # select random text
      selected_id <- sample(available_corpora, 1)
      
      # load preprocessed dataset 
      data <- full_corpus_list[[selected_id]]
      
      # culling
      data <- perform.culling(data, culling.level = methods_combination$culling[method])
      if(dim(data)[2] > methods_combination$MFU[method])
        data <- data[,1:methods_combination$MFU[method]]
      
      # apply the imposters
      imposters_results <- imposters(reference.set = data[-c(1,2),], test = data[1,], candidate.set = data[2,], distance = methods_combination$distance[method])
      
      # save incrementally
      full_results <- rbind(full_results, data.frame(configuration = methods_combination$configuration[method], repetition = ((candidate_id-1)*validation_rounds)+validation, analysis_type = "red", value = imposters_results))
      
    }
    
  }
  
  # cat("\n######\n######\n Actual analysis\n\n")
  
  available_corpus <- which(full_corpus_metadata$analysis_type == "actual" &
                                 full_corpus_metadata$impostors == methods_combination$n_imposters[method])
    
  for(validation in 1:validation_rounds){
    
    # load preprocessed dataset 
    data <- full_corpus_list[[available_corpus]]
    
    # culling
    data <- perform.culling(data, culling.level = methods_combination$culling[method])
    if(dim(data)[2] > methods_combination$MFU[method])
      data <- data[,1:methods_combination$MFU[method]]
    
    # apply the imposters
    imposters_results <- imposters(reference.set = data[-c(1,2),], test = data[1,], candidate.set = data[2,], distance = methods_combination$distance[method])
    
    # save incrementally
    full_results <- rbind(full_results, data.frame(configuration = methods_combination$configuration[method], repetition = validation, analysis_type = "actual", value = imposters_results))
    
  }
  
  # Save results incrementally
  
  write.csv(full_results, file = filename, row.names = F)

}