# get vector of result file names and extract job nums
find . -name "results_*.csv" -print | cut -d "_" -f 2 | cut -d "." -f 1 > completed_jobs


# compare to job_nums.txt to pull out missing jobs
awk 'FNR==NR {a[$0]++; next} !($0 in a)' completed_jobs job_nums.txt > job_nums_2.txt

# remove tmp file
rm completed_jobs
rm job_nums.txt

# rename new job file
mv job_nums_2.txt job_nums.txt
