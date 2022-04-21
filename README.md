# Traven_stylometry

Scripts and corpora for the paper *Traven between the impostors. Preliminary considerations on an authorship verification case*, presented at the DH2022 Conference.  

## Instructions

Run the *bash_imposters_number_parallel.sh*, which will run all R scripts in a sequence. Scripts are designed for parallel processing, to accelerate computation time.  
Analysis features are defined in the *analysis_features.csv* file.  
Texts to be analysed are in the *corpus* folder. The *essays* folder contains texts by additional authors, used to test the impostors method.  
**Note:** due to copyright reasons, texts in the *corpus* folder cannot be shared publicly (this repository contains just placeholders).

## Requirements

R packages: rvest, stringr, stylo, tidyverse.  
The bash script should run via command line on Unix-like systems.