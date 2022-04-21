#!/usr/bin/bash

# Step 1. Prepare imposters by selecting the best ones (closest to the analysed texts)
Rscript prepare_imposters_corpora.R

# Step 2. Prepare instructions for parallel processing based on analysis features
Rscript evaluate_parallel_processing.R

# save instructions in array
declare -a array=()
i=0

while IFS= read -r line; do
    array[i]=$line
    let "i++"
done < "parallel_processing_instructions.txt"

# Step 3. Prepare R environment
Rscript prepare_parallel_process_environment.R

# Step 4. Perform analysis by splitting it into multiple parallel processes
n_cores=$(<n_cores.txt)

if [[ "$n_cores" == 2 ]]; then
Rscript imposters_number_parallel.R "${array[0]}" &
Rscript imposters_number_parallel.R "${array[1]}"
fi

if [[ "$n_cores" == 3 ]]; then
Rscript imposters_number_parallel.R "${array[0]}" &
Rscript imposters_number_parallel.R "${array[1]}" &
Rscript imposters_number_parallel.R "${array[2]}"
fi

if [[ "$n_cores" == 4 ]]; then
Rscript imposters_number_parallel.R "${array[0]}" &
Rscript imposters_number_parallel.R "${array[1]}" &
Rscript imposters_number_parallel.R "${array[2]}" &
Rscript imposters_number_parallel.R "${array[3]}"
fi

if [[ "$n_cores" == 5 ]]; then
Rscript imposters_number_parallel.R "${array[0]}" &
Rscript imposters_number_parallel.R "${array[1]}" &
Rscript imposters_number_parallel.R "${array[2]}" &
Rscript imposters_number_parallel.R "${array[3]}" &
Rscript imposters_number_parallel.R "${array[4]}"
fi

if [[ "$n_cores" == 6 ]]; then
Rscript imposters_number_parallel.R "${array[0]}" &
Rscript imposters_number_parallel.R "${array[1]}" &
Rscript imposters_number_parallel.R "${array[2]}" &
Rscript imposters_number_parallel.R "${array[3]}" &
Rscript imposters_number_parallel.R "${array[4]}" &
Rscript imposters_number_parallel.R "${array[5]}"
fi

# Step 5. Conflate results into a single table
Rscript conflate_test_results.R

echo "Process complete!"