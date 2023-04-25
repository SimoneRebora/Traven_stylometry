### 05_process_results

library(tidyverse)
library(class)

### 0. Functions
# function for Shapiro test (normal distribution)
# it returns an error if values are all equal
shapiro_test <- function(x) {
  out <- tryCatch(
    {
      shapiro.test(x)$p.value
    },
    error=function(cond) {
      # message("Here's the original error message:")
      # message(cond)
      # Choose a return value in case of error
      return(1)
    },
    warning=function(cond) {
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NA)
    }
  )    
  return(out)
}


# normalization function (for machine learning)
nor <-function(x) 
{ 
  (x -min(x)+1e-10)/(max(x)-min(x)+1e-10)   
}

### 1. read results

# read list of methods
all_features <- read.csv("analysis_features.csv", stringsAsFactors = F)

# reconstruct full combinations
n_best_imposters <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "n_best_imposters"], " ")))
MFU_series <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "MFU_series"], " ")))
culling <- as.numeric(unlist(strsplit(all_features$value[all_features$feature == "culling"], " ")))
validation_rounds <- as.numeric(all_features$value[all_features$feature == "validation_rounds"])
distances <- unlist(strsplit(all_features$value[all_features$feature == "distances"], " "))
unit <- unlist(strsplit(all_features$value[all_features$feature == "unit"], " "))

methods_df <- expand.grid(n_best_imposters, MFU_series, culling, distances, unit, stringsAsFactors = FALSE)

colnames(methods_df) <- c("n_imposters", "MFU", "culling", "distance", "unit")
methods_df$configuration <- 1:length(methods_df$n_imposters)
methods_df <- methods_df[,c(6,1,5,2,3,4)]

# read all results (in multiple files)
all_results_files <- list.files(pattern = "Final_results_")

# join results in a single dataframe
results_df <- data.frame()

for(my_file in all_results_files){
  
  results_df_tmp <- read.csv(my_file, stringsAsFactors = F)
  results_df <- rbind(results_df, results_df_tmp)
  
}

# check progress
length(unique(results_df$configuration))/length(methods_df$configuration)

# reduce to just processed combinations
methods_df <- methods_df[which(methods_df$configuration %in% unique(results_df$configuration)),]

# check normal distributions (and save to file)

sink("Results.txt")
cat("# Proportion of normal distributions:\n")
for(my_group in c("red", "green", "actual")){
  
  cat(my_group, "")
  tmp_df <- results_df %>% filter(analysis_type == my_group)
  shapiros <- tmp_df %>%
    group_by(configuration) %>%
    summarise(shapiro = shapiro_test(value))
  
  cat(length(which(shapiros$shapiro > 0.05)) / length(shapiros$shapiro), "\n")
  
}
sink()

### 2. Results method 1 (thresholds)

methods_df$efficiency <- 0
methods_df$thresh_red <- 0
methods_df$thresh_green <- 0
methods_df$thresh_grey <- 0

for(i in methods_df$configuration){
  
  print(i)
  
  # filter dataframe via identifier
  analysis_df <- results_df %>%
    filter(configuration == i)
  
  analysis_df_actual <- analysis_df %>%
    filter(analysis_type == "actual")
  
  # find thresholds
  red_threshold <- quantile(analysis_df$value[which(analysis_df$analysis_type == "red")])[4]
  green_threshold <- quantile(analysis_df$value[which(analysis_df$analysis_type == "green")])[2]
  
  if(green_threshold < red_threshold){
    red_threshold_tmp <- green_threshold
    green_threshold <- red_threshold
    red_threshold <- red_threshold_tmp
  }
  
  methods_df$thresh_red[which(methods_df$configuration == i)] <- length(which(analysis_df_actual$value < red_threshold))
  methods_df$thresh_green[which(methods_df$configuration == i)] <- length(which(analysis_df_actual$value > green_threshold))
  methods_df$thresh_grey[which(methods_df$configuration == i)] <- length(which(analysis_df_actual$value >= red_threshold & analysis_df_actual$value <= green_threshold))
  
  # calculate efficiency
  red_score <- median(analysis_df$value[which(analysis_df$analysis_type == "red")])
  green_score <- median(analysis_df$value[which(analysis_df$analysis_type == "green")])
  
  methods_df$efficiency[which(methods_df$configuration == i)] <- green_score-red_score

}

# remove combinations with negative efficiency
if(length(which(methods_df$efficiency <= 0)) > 0)
  methods_df <- methods_df[-which(methods_df$efficiency <= 0),]

# weight for efficiency
methods_df$thresh_red <- methods_df$thresh_red * methods_df$efficiency
methods_df$thresh_green <- methods_df$thresh_green * methods_df$efficiency
methods_df$thresh_grey <- methods_df$thresh_grey * methods_df$efficiency

# calculate final proportions and save them to results file
sink(file = "Results.txt", append = T)
cat("# Thresholds results\n")
cat("\nConfirmed ", sum(methods_df$thresh_green) / sum(c(methods_df$thresh_red, methods_df$thresh_green, methods_df$thresh_grey)))
cat("\nNegated ", sum(methods_df$thresh_red) / sum(c(methods_df$thresh_red, methods_df$thresh_green, methods_df$thresh_grey)))
cat("\nUncertain ", sum(methods_df$thresh_grey) / sum(c(methods_df$thresh_red, methods_df$thresh_green, methods_df$thresh_grey)))
sink()

### 3. Results method 2 (knn)

methods_df$knn_green <- 0
methods_df$knn_red <- 0

for(i in methods_df$configuration){
  
  print(i)
  
  # filter dataframe via identifier
  analysis_df <- results_df %>%
    filter(configuration == i)
  
  # Run nomalization
  analysis_df_norm <- data.frame(value = nor(analysis_df[,4]))
  
  # extract training set
  train <- data.frame(value = analysis_df_norm[which(analysis_df$analysis_type %in% c("red", "green")),])
  # extract testing set
  test <- data.frame(value = analysis_df_norm[-which(analysis_df$analysis_type %in% c("red", "green")),]) 
  # extract analysis type of train dataset because it will be used as 'cl' argument in knn function.
  target_category <- analysis_df[which(analysis_df$analysis_type %in% c("red", "green")),3]
  target_category <- factor(target_category, levels = c("red", "green"))
  
  # run knn function
  pr <- knn(train, test, cl=target_category, k=13)
  
  # make table of results
  final_result <- table(pr)
  
  # save results to dataframe
  methods_df$knn_green[which(methods_df$configuration == i)] <- final_result[which(names(final_result) == "green")]
  methods_df$knn_red[which(methods_df$configuration == i)] <- final_result[which(names(final_result) == "red")]
  
}

# weight for efficiency

methods_df$knn_green <- methods_df$knn_green * methods_df$efficiency
methods_df$knn_red <- methods_df$knn_red * methods_df$efficiency

# calculate final proportions and save them to results file

sink(file = "Results.txt", append = T)
cat("\n\n# KNN\n")
cat("\nConfirmed ", sum(methods_df$knn_green) / sum(c(methods_df$knn_green, methods_df$knn_red)))
cat("\nNegated ", sum(methods_df$knn_red) / sum(c(methods_df$knn_green, methods_df$knn_red)))
sink()


### 3. Results method 3 (logistic regression)

methods_df$lr_green <- 0
methods_df$lr_red <- 0

for(i in methods_df$configuration){
  
  print(i)
  
  # filter dataframe via identifier
  analysis_df <- results_df %>%
    filter(configuration == i)
  
  # Run nomalization
  analysis_df_norm <- data.frame(value = nor(analysis_df[,4]))
  
  # extract training set
  train <- data.frame(value = analysis_df_norm[which(analysis_df$analysis_type %in% c("red", "green")),])
  # extract testing set
  test <- data.frame(value = analysis_df_norm[-which(analysis_df$analysis_type %in% c("red", "green")),]) 
  # extract analysis type of train dataset because it will be used as truth
  target_category <- analysis_df[which(analysis_df$analysis_type %in% c("red", "green")),3]
  target_category[which(target_category == "red")] <- 0
  target_category[which(target_category == "green")] <- 1
  train$truth <- as.numeric(target_category)
  
  # run logistic regression
  model <- glm( truth ~ value, data = train, family = binomial)
  prob <- model %>% predict(test, type = "response")
  pr <- ifelse(prob>0.5, "green", "red")

  # save results to dataframe
  methods_df$lr_green[which(methods_df$configuration == i)] <- length(which(pr == "green"))
  methods_df$lr_red[which(methods_df$configuration == i)] <- length(which(pr == "red"))
  
}

# weight for efficiency
methods_df$lr_green <- methods_df$lr_green * methods_df$efficiency
methods_df$lr_red <- methods_df$lr_red * methods_df$efficiency

# calculate final proportions and save them to results file

sink(file = "Results.txt", append = T)
cat("\n\n# KNN\n")
cat("\nConfirmed ", sum(methods_df$lr_green) / sum(c(methods_df$lr_green, methods_df$lr_red)))
cat("\nNegated ", sum(methods_df$lr_red) / sum(c(methods_df$lr_green, methods_df$lr_red)))
sink()
