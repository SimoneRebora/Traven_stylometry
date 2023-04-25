#!/usr/bin/bash

# Step 1. Prepare imposters and dev corpora
Rscript 01_prepare_imposters_corpora.R

# Step 2. Prepare instructions for parallel processing based on analysis features
Rscript 02_evaluate_parallel_processing.R

# Step 3. Prepare analysis tables (to speed up process and reduce RAM usage)
Rscript 03_prepare_analysis_tables.R

# save instructions in array
declare -a array=()
i=0

while IFS= read -r line; do
    array[i]=$line
    let "i++"
done < "parallel_processing_instructions.txt"

n_cores=$(<n_cores.txt)

# Step 4. Perform analysis by splitting it into multiple parallel processes
if [[ "$n_cores" == 2 ]]; then
Rscript 04_imposters_analysis.R "${array[0]}" &
Rscript 04_imposters_analysis.R "${array[1]}" &
wait
fi

if [[ "$n_cores" == 3 ]]; then
Rscript 04_imposters_analysis.R "${array[0]}" &
Rscript 04_imposters_analysis.R "${array[1]}" &
Rscript 04_imposters_analysis.R "${array[2]}" &
wait
fi

if [[ "$n_cores" == 4 ]]; then
Rscript 04_imposters_analysis.R "${array[0]}" &
Rscript 04_imposters_analysis.R "${array[1]}" &
Rscript 04_imposters_analysis.R "${array[2]}" &
Rscript 04_imposters_analysis.R "${array[3]}" &
wait
fi

if [[ "$n_cores" == 5 ]]; then
Rscript 04_imposters_analysis.R "${array[0]}" &
Rscript 04_imposters_analysis.R "${array[1]}" &
Rscript 04_imposters_analysis.R "${array[2]}" &
Rscript 04_imposters_analysis.R "${array[3]}" &
Rscript 04_imposters_analysis.R "${array[4]}" &
wait
fi

if [[ "$n_cores" == 6 ]]; then
Rscript 04_imposters_analysis.R "${array[0]}" &
Rscript 04_imposters_analysis.R "${array[1]}" &
Rscript 04_imposters_analysis.R "${array[2]}" &
Rscript 04_imposters_analysis.R "${array[3]}" &
Rscript 04_imposters_analysis.R "${array[4]}" &
Rscript 04_imposters_analysis.R "${array[5]}" &
wait
fi

# Step 5. Process and write final results
Rscript 05_process_results.R

echo "Process complete!"