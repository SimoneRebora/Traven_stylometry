# Traven_stylometry

Scripts and corpora for the authorship attribution of B. Traven's works, using the method of the "impostors".

## Structure
The main branch of the repository contains materials for the paper *Are Ret Marut and B. Traven the same person? Fine tuning the impostors method*, presented at the [DH2023 Conference](https://dh2023.adho.org/) ([Paper](https://zenodo.org/record/7961822) | [Slides](https://docs.google.com/presentation/d/1yvF_JrCwsM0mzdQ7sLknZO9oxqr8d3ueFpgO0YHKI-0/edit?usp=sharing)).  
The [DH2022 branch](https://github.com/SimoneRebora/Traven_stylometry/tree/DH2022) contains materials for the paper *Traven between the impostors. Preliminary considerations on an authorship verification case*, presented at the [DH2022 Conference](https://dh2022.adho.org/) ([Paper](https://dh2022.dhii.asia/dh2022bookofabsts.pdf#page=546)).  

## Instructions

Run the *bash_imposters_parallel.sh* file (via: `bash bash_imposters_parallel.sh`), which will run all R scripts in a sequence. Scripts are designed for parallel processing, to accelerate computation speed.  

### Features

Analysis features are defined in the *analysis_features.csv* file. You can modify them to run different analyses:
- **n_cores** defines the number of cores for parallel processing (the script currently supports from two to six cores)
- **n_best_imposters** defines the number(s) of best impostors on which to run tests (you should separate the numbers with a space) 
- **n_development_authors** defines the number of authors to consitute the development set
- **unit** defines the unit of analysis. You can choose between "1_words", "2_characters", "3_characters", etc. (currently, the script does not support word ngrams and runs with just one unit at the time)
- **MFU_series** defines the number(s) of most frequent units (words or characters) on which to run tests (you should separate the numbers with a space) 
- **culling** defines the level(s) of culliung with which to run tests (you should separate the numbers with a space) 
- **validation_rounds** defines the number of repetitions for each configuration
- **distances** defines the stylometric distances to be used (you should separate the names with a space)

### Scripts

- **01_prepare_imposters_corpora.R** prepares corpora by running a first stylometric analysis and selecting the authors closest to the test set
- **02_evaluate_parallel_processing.R** reads analysis features from the *analysis_features.csv* file and prepares instructions for parallel processing
- **03_prepare_analysis_tables.R** prepares datasets for the actual analysis, by creating Term-Document-Frequency tables for each combination of texts
- **04_imposters_analysis.R** performs the impostors analysis
- **05_process_results.R** conflates the results and saves them to a *Results.txt* file

### Corpora

Texts to be analysed are in the *corpus* folder:
- **Traven_Marut_corpus.RData** contains the four novels by Traven and Marut on which to perform the analysis. Novels have been split into tokens, which have been reordered alphabetically (thus not allowing reconstruction of the original texts, which are still copyright protected)
- **Kolimo_metadata.csv** contains metadata of the Kolimo corpus, from which development set and impostors will be extracted. The corpus itself will be downloaded by the R scripts

## Requirements

R packages: stylo, tidyverse, and class. Run the **Requirements.R** script to install them.  
The bash script should run via command line on Unix-like systems.
