# prepare_imposters_corpora

library(rvest)
library(stylo)

all_features <- read.csv("analysis_features.csv", stringsAsFactors = F)

n_best_imposters <- max(as.numeric(unlist(strsplit(all_features$value[all_features$feature == "n_best_imposters"], " "))))
n_development_authors <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "n_development_authors"], " ")))

### 1. Define functions

### Cleaning function
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

# tokenization function
text_process <- function(my_texts){
  texts_stylo <- list()
  for(i in 1:length(my_texts)){
    texts_stylo[[i]] <- stylo::txt.to.words.ext(input.text = my_texts[i], corpus.lang = "German")
    print(i)
  }
  return(texts_stylo)
}


### 2. Prepare corpora

# create Traven/Marut textoid by simply collapsing their novels to a single list of tokens
load("corpus/Traven_Marut_corpus.RData")
two_authors_collapsed <- unlist(traven_marut_full)

# create imposters corpus

download.file("https://owncloud.gwdg.de/index.php/s/C4voZFKJQYH8r3K/download", destfile = "corpus/Kolimo_corpus.RData")
load("corpus/Kolimo_corpus.RData")

### Prepare texts
full_metadata_withDates <- read.csv("corpus/Kolimo_metadata.csv", stringsAsFactors = F)
all_texts <- Kolimo_texts[which(full_metadata_withDates$date > 1880)]
full_metadata_withDates <- full_metadata_withDates[which(full_metadata_withDates$date > 1880),]

### Exclude (possible) duplicated titles
duplicated_testing <- paste(full_metadata_withDates$gnd_id, full_metadata_withDates$title)
exclude <- which(duplicated(duplicated_testing))
if(length(exclude) > 0){
  all_texts <- all_texts[-exclude]
  full_metadata_withDates <- full_metadata_withDates[-exclude,]
}

### clean imposters texts
all_texts <- clean_texts(all_texts)

### Verify that our authors are not in the imposters corpus
which(grepl(pattern = "Traven", full_metadata_withDates$author))
which(grepl(pattern = "Marut", full_metadata_withDates$author))
which(full_metadata_withDates$gnd_id == "http://d-nb.info/gnd/118623672") # Traven's GND

### Tokenize imposters texts 
full_corpus_tmp <- text_process(all_texts)

### collapse all texts by the same imposter
full_corpus <-  list()
all_imposters <- unique(full_metadata_withDates$gnd_id)
for(i in 1:length(all_imposters)){
  
  my_imposter_texts <- which(full_metadata_withDates$gnd_id == all_imposters[i])
  full_corpus[[i]] <- unlist(full_corpus_tmp[my_imposter_texts])
  
}

length_limit <- 5000
texts_l <- unlist(lapply(full_corpus, length))
good_length <- which(texts_l >= length_limit)
full_corpus <- full_corpus[good_length]
all_imposters <- all_imposters[good_length]
names(full_corpus) <- all_imposters
remove(full_corpus_tmp)

### 3. Find the closest impostors

custom.txt.collection <- c(test = list(two_authors_collapsed), full_corpus)

stylo_results <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=2000, mfw.max=2000, mfw.incr=0, distance.measure="dist.wurzburg", write.pdf.file = FALSE, parsed.corpus = custom.txt.collection)

distances_test <- sort(stylo_results$distance.table[1,])

selected_kolimo <- names(distances_test)[2:(n_development_authors+1)]
selected_kolimo_imposters <- names(distances_test)[(n_development_authors+2):((n_development_authors+2+n_best_imposters))]

kolimo_dev <- full_corpus[which(names(full_corpus) %in% selected_kolimo)]
kolimo_dev <- kolimo_dev[selected_kolimo]

kolimo_imposters <- full_corpus[which(names(full_corpus) %in% selected_kolimo_imposters)]
kolimo_imposters <- kolimo_imposters[selected_kolimo_imposters]

save(kolimo_imposters, file = "Imposters_set.RData")
save(kolimo_dev, file = "Development_set.RData")
