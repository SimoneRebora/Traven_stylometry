### imposters_testing

library(tidyverse)
library(stylo)

my_files <- list.files(pattern = "Imposters_test")
my_files <- my_files[which(grepl(pattern = ".RData", x = my_files))]

my_authors <- c("Feige",  "Marut",  "Traven")

verifications_f <- vector(mode = "list", length = length(my_authors))
convalidations_f <- vector(mode = "list", length = length(my_authors))
verifications_mfw_f <- vector(mode = "list", length = length(my_authors))
convalidations_mfw_f <- vector(mode = "list", length = length(my_authors))

for(my_file in my_files){
  
  load(my_file)
  for(i in 1:length(verifications_f))
    verifications_f[[i]] <- c(verifications_f[[i]], verifications[[i]])
  for(i in 1:length(convalidations_f))
    convalidations_f[[i]] <- c(convalidations_f[[i]], convalidations[[i]])
  for(i in 1:length(verifications_mfw_f))
    verifications_mfw_f[[i]] <- c(verifications_mfw_f[[i]], verifications_mfw[[i]])
  for(i in 1:length(convalidations_mfw_f))
    convalidations_mfw_f[[i]] <- c(convalidations_mfw_f[[i]], convalidations_mfw[[i]])
  
}
  
verifications <- verifications_f
convalidations <- convalidations_f
verifications_mfw <- verifications_mfw_f
convalidations_mfw <- convalidations_mfw_f

preparation <- function(my_authors){
  
  for(i in 1:length(my_authors)){
    
    verifications[[i]] <- split(verifications[[i]], ceiling(seq_along(verifications[[i]])/validation_rounds))
    verifications_mfw[[i]] <- split(verifications_mfw[[i]], ceiling(seq_along(verifications_mfw[[i]])/validation_rounds))
    
    verifications[[i]] <- lapply(verifications[[i]], function(x) c(mean(x), sd(x)))
    verifications_mfw[[i]] <- lapply(verifications_mfw[[i]], function(x) c(mean(x), sd(x)))
    
    convalidations[[i]] <- split(convalidations[[i]], ceiling(seq_along(convalidations[[i]])/validation_rounds))
    convalidations_mfw[[i]] <- split(convalidations_mfw[[i]], ceiling(seq_along(convalidations_mfw[[i]])/validation_rounds))
    
    convalidations[[i]] <- lapply(convalidations[[i]], function(x) c(mean(x), sd(x)))
    convalidations_mfw[[i]] <- lapply(convalidations_mfw[[i]], function(x) c(mean(x), sd(x)))
    
  }
  
  results_per_method <- methods_combination[1:length(verifications[[1]]),]
  
  for(i in 1:length(verifications)){
    
    tmp_df <- cbind(do.call(rbind.data.frame, verifications[[i]]), do.call(rbind.data.frame, convalidations[[i]]))
    colnames(tmp_df) <- c(my_authors[i], paste(my_authors[i], "sd", sep = "_"), paste("NOT", my_authors[i], sep = "_"), paste("NOT", my_authors[i], "sd", sep = "_"))
    results_per_method <- cbind(results_per_method, tmp_df)
    
  }
  
  validation_mean <- numeric()
  validation_sd <- numeric()
  validation_negative_mean <- numeric()
  validation_negative_sd <- numeric()
  mfw_mean <- numeric()
  mfw_sd <- numeric()
  
  for(i in 1:length(verifications[[i]])){
    
    all_values <- unlist(lapply(verifications, function(x) x[i]), recursive = F)
    validation_mean[i] <- mean(unlist(lapply(all_values, function(x) x[1])))
    validation_sd[i] <- mean(unlist(lapply(all_values, function(x) x[2])))
    
    all_values <- unlist(lapply(convalidations, function(x) x[i]), recursive = F)
    validation_negative_mean[i] <- mean(unlist(lapply(all_values, function(x) x[1])))
    validation_negative_sd[i] <- mean(unlist(lapply(all_values, function(x) x[2])))
    
    all_values <- c(unlist(lapply(verifications_mfw, function(x) x[i]), recursive = F), unlist(lapply(convalidations_mfw, function(x) x[i]), recursive = F))
    mfw_mean[i] <- mean(unlist(lapply(all_values, function(x) x[1])))
    mfw_sd[i] <- mean(unlist(lapply(all_values, function(x) x[2])))
    
  }
  
  results_per_method <- cbind(results_per_method, validation_mean, validation_sd, validation_negative_mean, validation_negative_sd, mfw_mean, mfw_sd)
  
  results_per_method$quality <- results_per_method$validation_mean - results_per_method$validation_negative_mean
  
  results_per_method$quality_sd <- rowMeans(results_per_method[,c("validation_sd","validation_negative_sd")])
  
  return(results_per_method)
  
}

results_per_method <- preparation(my_authors)

my_time <- gsub("\\W", "-", Sys.time())
filename <- paste("Final_results__date", my_time, ".csv", sep = "")
results_per_method <- results_per_method[order(as.numeric(rownames(results_per_method))),]
write.csv(results_per_method, file = filename)
save.image(gsub(pattern = ".csv", replacement = ".RData", x = filename))