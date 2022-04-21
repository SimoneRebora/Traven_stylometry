### prepare_parallel_process_environment

library(stylo)

# functions

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

# read texts by three authors

all_files <- list.files("corpus", full.names = T)

three_authors <- list()

for(i in 1:length(all_files)){
  
  tmp_text <- readLines(all_files[i])
  tmp_text <- paste(tmp_text, collapse = " ")
  tmp_text <- clean_texts(tmp_text)
  three_authors[[i]] <- stylo::txt.to.words.ext(tmp_text, corpus.lang = "German")
  
  print(i)
  
}

candidates_corpus <- list()
reference_corpus <- list()

for(i in 1:length(three_authors)){
  
  candidates_corpus[[i]] <- three_authors[[i]][1:(round(length(three_authors[[i]])/2))]
  reference_corpus[[i]] <- three_authors[[i]][(round(length(three_authors[[i]])/2)+1):length(three_authors[[i]])]
  
}

names(candidates_corpus) <- gsub(".txt", "", list.files("corpus"))
names(reference_corpus) <- gsub(".txt", "", list.files("corpus"))


# read texts by other authors

all_files <- list.files("essays", full.names = T, recursive = T)
other_author_names <- strsplit(all_files, "/")
other_author_names <- sapply(other_author_names, function(x) x[2])
other_author_sel <- unique(other_author_names)

other_authors <- list()

for(i in 1:length(other_author_sel)){
  
  sel_files <- which(other_author_names == other_author_sel[i])
  tmp_text_full <- character()
  
  for(my_file in all_files[sel_files]){
    
    tmp_text <- readLines(my_file)
    tmp_text <- paste(tmp_text, collapse = " ")
    tmp_text <- clean_texts(tmp_text)
    tmp_text_full <- paste(tmp_text_full, tmp_text)
    
  }
  
  other_authors[[i]] <- stylo::txt.to.words.ext(tmp_text_full, corpus.lang = "German")
  
  print(i)
  
}

# split other authors' texts into 5,000-word chuncks
text_process <- function(my_texts, length_limit = 5000){
  texts_split <- split(my_texts, ceiling(seq_along(my_texts)/length_limit))
  print(length(texts_split[[length(texts_split)]]))
  texts_split <- texts_split[-length(texts_split)]
  if(length(texts_split) < 3)
    cat("ERROR!!!!!\nSamples are too few!!!\n!!!!!")
  return(texts_split)
} 

for(i in 1:length(other_authors)){
  other_authors[[i]] <- text_process(other_authors[[i]])
  names(other_authors[[i]]) <- paste(other_author_sel[i], 1:length(other_authors[[i]]), sep = "_")
}

other_authors <- unlist(other_authors, recursive = F)

# for each author, find the 5,000 word chunks that are the closest to the candidates
three_authors_collapsed <- unlist(three_authors)
custom.txt.collection <- c(test = list(three_authors_collapsed), other_authors)

stylo_results <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=2000, mfw.max=2000, mfw.incr=0, distance.measure="dist.wurzburg", write.pdf.file = FALSE, parsed.corpus = custom.txt.collection)

distances_test <- sort(stylo_results$distance.table[1,])
other_author_test <- strsplit(names(distances_test), "_")
other_author_test <- sapply(other_author_test, function(x) x[1])

selected_authors <- character()
for(i in 1:length(other_author_sel)){
  
  selected_authors <- c(selected_authors, names(distances_test)[which(other_author_test == other_author_sel[i])[1:3]])
  
}

other_authors <- other_authors[which(names(other_authors) %in% selected_authors)]

save.image("Parallel_processes_environment.RData")
