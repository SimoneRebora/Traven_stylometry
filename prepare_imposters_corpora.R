# prepare_imposters_corpora

library(rvest)
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

### additional function (for culling)
there_is_zero <- function(x){
  if(0 %in% x){
    return(TRUE)
  }else{
    return(FALSE)
  }
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


# read the Zeitungen corpus
# download corpus
download.file("https://owncloud.gwdg.de/index.php/s/WZUFwVcQy01zxlk/download", destfile = "Zeitungen_corpus.zip")
unzip("Zeitungen_corpus.zip")

all_files <- list.files("Zeitungen_corpus", pattern = ".xml", full.names = T)

zeitungen <- list()

for(i in 1:length(all_files)){
  
  my_xml <- read_html(all_files[i])
  
  tmp_text <- my_xml %>% html_node(xpath = "//text") %>% html_text()
  
  tmp_text <- paste(tmp_text, collapse = " ")
  tmp_text <- clean_texts(tmp_text)
  zeitungen[[i]] <- stylo::txt.to.words.ext(tmp_text, corpus.lang = "German")
  
  print(i)
  
}

zeitungen <- zeitungen[which(lengths(zeitungen) > 5000)]

names(zeitungen) <- paste("Zeitungen", 1:length(zeitungen), sep = "")

# create imposters corpus

download.file("https://owncloud.gwdg.de/index.php/s/b7umCKWXFY2AbVh/download", destfile = "Kolimo_corpus.RData")
download.file("https://owncloud.gwdg.de/index.php/s/627ruOt6gE4uDiY/download", destfile = "Kolimo_metadata.RData")
load("Kolimo_corpus.RData")
load("Kolimo_metadata.RData")

text_process <- function(my_texts){
  texts_stylo <- list()
  for(i in 1:length(my_texts)){
    texts_stylo[[i]] <- stylo::txt.to.words.ext(input.text = my_texts[i], corpus.lang = "German")
    print(i)
  }
  return(texts_stylo)
}

### Prepare texts
full_metadata_withDates <- full_metadata_withDates[1:6100,]
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

### Separate candidate authors from imposters
candidates <- c("http://d-nb.info/gnd/118585916", # Musil
                "http://d-nb.info/gnd/118530909", # Ernst
                "http://d-nb.info/gnd/118584758", # Mühsam
                "http://d-nb.info/gnd/118603590", # Rubiner
                "http://d-nb.info/gnd/118623672" # Traven
)

# NOTE: add Hermann Bahr? "http://d-nb.info/gnd/118505955"

exclude <- which(full_metadata_withDates$gnd_id %in% candidates)
all_texts <- all_texts[-exclude]
full_metadata_withDates <- full_metadata_withDates[-exclude,]

### clean imposters texts 
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


### Find the closest 40 impostors

# in Zeitungen

### Select best imposters (through stylometric analysis)
three_authors_collapsed <- unlist(three_authors)
custom.txt.collection <- c(test = list(three_authors_collapsed), zeitungen)
  
stylo_results <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=2000, mfw.max=2000, mfw.incr=0, distance.measure="dist.wurzburg", write.pdf.file = FALSE, parsed.corpus = custom.txt.collection)

distances_test <- sort(stylo_results$distance.table[1,])
selected_zeitungen <- names(distances_test)[2:41]

zeitungen <- zeitungen[which(names(zeitungen) %in% selected_zeitungen)]

# in Kolimo

### Select best imposters (through stylometric analysis)
custom.txt.collection <- c(test = list(three_authors_collapsed), full_corpus)

stylo_results <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=2000, mfw.max=2000, mfw.incr=0, distance.measure="dist.wurzburg", write.pdf.file = FALSE, parsed.corpus = custom.txt.collection)

distances_test <- sort(stylo_results$distance.table[1,])
selected_kolimo <- names(distances_test)[2:41]

kolimo <- full_corpus[which(names(full_corpus) %in% selected_kolimo)]

save(zeitungen, kolimo, 
     selected_zeitungen, selected_kolimo,
     file = "Imposters.RData")