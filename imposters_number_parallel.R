# imposters_number_parallel

library(stylo)
library(stringr)
load("Imposters.RData")
load("Parallel_processes_environment.RData")

# Variables

methods_combination <- read.csv("Methods_combination_parallel.csv", stringsAsFactors = F, row.names = 1)

args <- commandArgs(trailingOnly=TRUE)
args_split <- unlist(strsplit(args, " "))

start_loop <- as.numeric(args_split[1])
end_loop <- as.numeric(args_split[2])
validation_rounds <- as.numeric(args_split[3])

# start main impostors loop
filename <- paste("Imposters_test__par", str_pad(start_loop, 8, pad = "0"), ".txt", sep = "")

cat("Imposters results\n\n", file = filename)

verifications <- vector(mode = "list", length = length(three_authors))
convalidations <- vector(mode = "list", length = length(three_authors))
verifications_mfw <- vector(mode = "list", length = length(three_authors))
convalidations_mfw <- vector(mode = "list", length = length(three_authors))

for(method in start_loop:end_loop){
  
  cat("\n\nConfiguration:", methods_combination$n_imposters[method], "imposters", methods_combination$MFW[method], "MFW", methods_combination$corpus[method], "corpus", methods_combination$distance[method], "culling", methods_combination$culling[method], "\n\n", file = filename, append = T)
  
  full_corpus <- get(methods_combination$corpus[method])
  
  for(validation in 1:validation_rounds){
    
    ### Select best imposters (random)
    full_corpus_tmp <- full_corpus[sample(1:length(full_corpus), methods_combination$n_imposters[method], replace = F)]
    
    cat("\n\nValidation", validation, "\n\n", file = filename, append = T)
    
    ### run imposters analysis
    for(i in 1:length(candidates_corpus)){
      
      candidate_stylo <- reference_corpus[i]
      my_candidate <- names(reference_corpus)[i]
      
      ### Prepare full corpus for analysis 
      imposters_corpus <- c(test = list(candidates_corpus[[i]]), candidate_stylo, full_corpus_tmp)
      
      test_id <- which(names(imposters_corpus)=="test")
      candidate_id <- which(grepl(x = names(imposters_corpus), pattern = my_candidate))
      
      # computing a list of most frequent words (trimmed to top 2000 items):
      features = make.frequency.list(imposters_corpus, head = 2000)
      
      # producing a table of relative frequencies:
      data = make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
      
      # culling
      data <- perform.culling(data, culling.level = methods_combination$culling[method])
      if(dim(data)[2] > methods_combination$MFW[method])
        data <- data[,1:methods_combination$MFW[method]]
      
      # who wrote the test text? (in my case, this is the 1st row in the table):
      imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = methods_combination$distance[method])
      
      cat(my_candidate, " probability: ", imposters_results, "\n", file = filename, sep = "", append = T)
      
      verifications[[i]] <- c(verifications[[i]], imposters_results)
      verifications_mfw[[i]] <- c(verifications_mfw[[i]], dim(data)[2])
      
      # testing on the wrong author (Imposters should negate the attribution)
      
      ### Prepare full corpus for analysis
      my_other_author <- sample(1:length(other_authors), 1)
      imposters_corpus <- c(test = list(other_authors[[my_other_author]]), candidate_stylo, full_corpus_tmp)
      
      test_id <- which(names(imposters_corpus)=="test")
      candidate_id <- which(grepl(x = names(imposters_corpus), pattern = my_candidate))
      
      # computing a list of most frequent words (trimmed to top 2000 items):
      features = make.frequency.list(imposters_corpus, head = 2000)
      
      # producing a table of relative frequencies:
      data = make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
      
      # culling
      data <- perform.culling(data, culling.level = methods_combination$culling[method])
      if(dim(data)[2] > methods_combination$MFW[method])
        data <- data[,1:methods_combination$MFW[method]]
      
      # who wrote the test text? (in my case, this is the 1st row in the table):
      imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = methods_combination$distance[method])
      
      cat("Not ", my_candidate, " probability: ", imposters_results, "\n", file = filename, sep = "", append = T)
      
      convalidations[[i]] <- c(convalidations[[i]], imposters_results)
      convalidations_mfw[[i]] <- c(convalidations_mfw[[i]], dim(data)[2])
      
      cat("method:", (method-start_loop+1)/(end_loop-start_loop+1), "validation:", validation/validation_rounds, "\n", file = paste("progress_par", str_pad(start_loop, 8, pad = "0"), ".log", sep = ""))
      
    }
    
  }
  
  save(verifications, convalidations, verifications_mfw, convalidations_mfw, methods_combination, validation_rounds, file = gsub(".txt", ".RData", filename))
  
}
