### Imposters_prepare_tables

library(stylo)
library(tidyverse)

all_features <- read.csv("analysis_features.csv", stringsAsFactors = F)

n_best_imposters <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "n_best_imposters"], " ")))
full_unit <- unlist(strsplit(all_features$value[all_features$feature == "unit"], " "))

full_unit_split <- unlist(strsplit(full_unit, "_"))

my_unit <- full_unit_split[2]
my_ngram_size <- as.numeric(full_unit_split[1])

### 0. Define Functions

# Cleaning function
clean_texts <- function(my_texts){
  for(i in 1:length(my_texts)){
    Kolimo_string <- my_texts[i]
    ##substitute old S
    Kolimo_string <- gsub("ſ", "s", Kolimo_string)
    ##substitute umlauts and double S
    Kolimo_string <- gsub("ä", "ae", Kolimo_string)
    Kolimo_string <- gsub("ö", "oe", Kolimo_string)
    Kolimo_string <- gsub("ü", "ue", Kolimo_string)
    Kolimo_string <- gsub("ß", "ss", Kolimo_string)
    
    ##substitute line breaks
    Kolimo_string <- gsub("([a-z])-\\s+([a-z])", "\\1\\2", Kolimo_string)
    Kolimo_string <- gsub("([a-z])¬\\s+([a-z])", "\\1\\2", Kolimo_string)
    
    ##remove soft hyphen
    Kolimo_string <- gsub(pattern = "\u00AD", replacement = "", Kolimo_string)
    
    ##delete multiple spaces
    Kolimo_string <- gsub(pattern = " +", replacement = " ", x = Kolimo_string)
    
    ##save new version
    my_texts[i] <- Kolimo_string
    if(length(my_texts) == 1){
      print("Cleaned")
    }else{
      print(i)
    }
    
  }
  return(my_texts)
}

# Splitting function
text_process <- function(my_texts, length_limit = 5000){
  texts_split <- split(my_texts, ceiling(seq_along(my_texts)/length_limit))
  print(length(texts_split[[length(texts_split)]]))
  texts_split <- texts_split[-length(texts_split)]
  return(texts_split)
} 

# n-grams creation
ngrams_creation <- function(corpus_tmp, my_ngram_size){
  corpus_tmp_ngrams <- unlist(lapply(corpus_tmp, function(x) paste(x, collapse = " ")))
  corpus_tmp_ngrams <- strsplit(corpus_tmp_ngrams, "")
  corpus_tmp_ngrams <- lapply(corpus_tmp_ngrams, function(x) make.ngrams(x, my_ngram_size))
  return(corpus_tmp_ngrams)
}


### 1. Prepare corpora

# Traven and Marut (load already processed corpora)
load("corpus/Traven_Marut_corpus.RData")

# load development set
load("Development_set.RData")

# development set will be split into 5,000 word chuncks
development_set <- list()
for(i in 1:length(kolimo_dev)){
  development_set[[i]] <- text_process(kolimo_dev[[i]])
  names(development_set)[i] <- names(kolimo_dev)[i]
}

# load imposters set
load("Imposters_set.RData")

# creation of n-grams versions
kolimo_imposters_ngrams <- ngrams_creation(kolimo_imposters, my_ngram_size)
development_set_ngrams <- lapply(development_set, function(x) ngrams_creation(x, my_ngram_size))

### 2. Dataset creation

# prepare variables for dataset creation
full_corpus_list <- list()
full_corpus_metadata <- data.frame()
counter <- 1

# first loop with dev set

for(dev_id in 1:length(development_set)){
  
  cat("\n#####\n#####\n##### Processing dev no.", dev_id, "\n\n")

  for(dev_text_id in 1:length(development_set[[dev_id]])){
    
    cat("\n#####\n##### Processing text no.", dev_text_id, "\n\n")
    
    for(n_imposters in n_best_imposters){
      
      cat("\n##### Processing", n_imposters, "impostors\n\n")
      
      if(my_unit == "words")
        full_corpus_tmp <- kolimo_imposters[1:n_imposters]
      if(my_unit == "characters")
        full_corpus_tmp <- kolimo_imposters_ngrams[1:n_imposters]
      
      for(i in 1:length(candidates_corpus)){
        
        if(my_unit == "words")
          candidate_stylo <- candidates_corpus[i]
        if(my_unit == "characters")
          candidate_stylo <- candidates_corpus_ngrams[i]
        
        # prepare full corpus for analysis
        my_other_author <- dev_id
        
        if(my_unit == "words")
          my_test <- development_set[[my_other_author]]
        if(my_unit == "characters")
          my_test <- development_set_ngrams[[my_other_author]]
        
        my_test <- unlist(my_test[dev_text_id])
        imposters_corpus <- c(test = list(my_test), candidate_stylo, full_corpus_tmp)
        
        # computing a list of most frequent words (trimmed to top 2000 items):
        features <- make.frequency.list(imposters_corpus, head = 2000)
        
        # producing a table of relative frequencies:
        data <- make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
        
        # save it incrementally
        full_corpus_list[[counter]] <- data
        counter <- counter + 1
        
        # save metadata
        full_corpus_metadata <- rbind(full_corpus_metadata, data.frame(author_id = dev_id, text_id = dev_text_id, candidate_id = i, impostors = n_imposters, analysis_type = "convalidations", unit = full_unit))

      }
      
    }
    
  }
  
}

# second loop with test set

for(cand_id in 1:length(test_corpus)){
  
  cat("\n#####\n#####\n##### Processing candidate no.", cand_id, "\n\n")
  
  for(cand_text_id in 1:length(test_corpus[[cand_id]])){
    
    cat("\n#####\n##### Processing text no.", cand_text_id, "\n\n")
    
    for(n_imposters in n_best_imposters){
      
      cat("\n##### Processing", n_imposters, "impostors\n\n")
      
      if(my_unit == "words")
        full_corpus_tmp <- kolimo_imposters[1:n_imposters]
      if(my_unit == "characters")
        full_corpus_tmp <- kolimo_imposters_ngrams[1:n_imposters]
      
      if(my_unit == "words")
        candidate_stylo <- candidates_corpus[cand_id]
      if(my_unit == "characters")
        candidate_stylo <- candidates_corpus_ngrams[cand_id]
      
      # prepare full corpus for analysis
      if(my_unit == "words")
        my_test <- test_corpus[[cand_id]]
      if(my_unit == "characters")
        my_test <- test_corpus_ngrams[[cand_id]]
      
      my_test <- unlist(my_test[cand_text_id])
      
      imposters_corpus <- c(test = list(my_test), candidate_stylo, full_corpus_tmp)
      
      # computing a list of most frequent words (trimmed to top 2000 items):
      features <- make.frequency.list(imposters_corpus, head = 2000)
      
      # producing a table of relative frequencies:
      data <- make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
      
      # save it incrementally
      full_corpus_list[[counter]] <- data
      counter <- counter + 1
      
      # save metadata
      full_corpus_metadata <- rbind(full_corpus_metadata, data.frame(author_id = cand_id, text_id = cand_text_id, candidate_id = cand_id, impostors = n_imposters, analysis_type = "verifications", unit = full_unit))
    
    }
    
  }
  
}

# third loop with actual analysis
for(n_imposters in n_best_imposters){
      
  cat("\n##### Processing", n_imposters, "impostors\n\n")
  
  if(my_unit == "words")
    full_corpus_tmp <- kolimo_imposters[1:n_imposters]
  if(my_unit == "characters")
    full_corpus_tmp <- kolimo_imposters_ngrams[1:n_imposters]
  
  if(my_unit == "words")
    author_1 <- actual_corpus[1]
  if(my_unit == "characters")
    author_1 <- actual_corpus_ngrams[1]
  
  if(my_unit == "words")
    author_2 <- actual_corpus[2]
  if(my_unit == "characters")
    author_2 <- actual_corpus_ngrams[2]
  
  imposters_corpus <- c(author_1, author_2, full_corpus_tmp)
  
  # computing a list of most frequent words (trimmed to top 2000 items):
  features <- make.frequency.list(imposters_corpus, head = 2000)
  
  # producing a table of relative frequencies:
  data <- make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
  
  # save it incrementally
  full_corpus_list[[counter]] <- data
  counter <- counter + 1
  
  # save metadata
  full_corpus_metadata <- rbind(full_corpus_metadata, data.frame(author_id = 1, text_id = 1, candidate_id = 2, impostors = n_imposters, analysis_type = "actual", unit = full_unit))
      
}

save(full_corpus_list, full_corpus_metadata, file = paste("Preprocessed_dataset_", full_unit, ".RData", sep = ""))
