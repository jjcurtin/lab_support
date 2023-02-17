convert_log <- function (log_file, jobs_file) {
  
  # function takes in a text CHTC log file and jobs file and outputs a tibble with one row per job
  
  # convert text file to tibble
  log <- enframe(log_file, name = NULL, value = "raw_text") 
  
  # pull out job submissions
  job_start <- log %>% 
    filter(str_detect(raw_text, "Job submitted from host:")) 
  
  # get job number
  job_start <- job_start %>% 
    rowwise() %>% 
    mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) 
  
  # get submission time
  job_start <- job_start %>% 
    mutate(submission_dttm = str_split(str_split(raw_text, "\\) ")[[1]][2], " Job submitted")[[1]][1],
           submission_dttm = as_datetime(submission_dttm, tz = "America/Chicago")) %>% 
    ungroup()
  
  # pull out job executions
  job_execute <- log %>% 
    filter(str_detect(raw_text, "Job executing on host:")) 
  
  # get job number
  job_execute <- job_execute %>% 
    rowwise() %>% 
    mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) 
  
  # get execution time
  job_execute <- job_execute %>% 
    mutate(execution_dttm = str_split(str_split(raw_text, "\\) ")[[1]][2], " Job executing")[[1]][1],
           execution_dttm = as_datetime(execution_dttm, tz = "America/Chicago")) %>% 
    ungroup()
  
  # only keep last execution time (duplicates may be caused from held and released jobs)
  job_execute <- job_execute %>% 
    group_by(job_num) %>% 
    arrange(desc(execution_dttm)) %>% 
    slice(1) %>% 
    ungroup()
  
  # merge into final log
  final_log <- job_start %>% 
    select(-raw_text) %>% 
    full_join(job_execute %>% 
                select(-raw_text), by = c("job_num"))
  
  # pull out job terminations
  job_terminate <- log %>% 
    filter(str_detect(raw_text, "Job terminated") & str_detect(raw_text, "^[0-9]") )
  
  # get job number
  job_terminate <- job_terminate %>% 
    rowwise() %>% 
    mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) 
  
  # get termination time
  job_terminate <- job_terminate %>% 
    mutate(termination_dttm = str_split(str_split(raw_text, "\\) ")[[1]][2], " Job terminated")[[1]][1],
           termination_dttm = as_datetime(termination_dttm, tz = "America/Chicago")) %>% 
    ungroup()
  
  # merge into final log
  final_log <- final_log %>% 
    full_join(job_terminate %>% 
                select(-raw_text), by = c("job_num"))
  
  # calculate total run time
  final_log <- final_log %>% 
    mutate(run_time = difftime(termination_dttm, execution_dttm, units = "mins"))
  
  
  # get CPU usage, disk usage, memory usage
  row_indexes <- which(log$raw_text %in% job_terminate$raw_text)
  
  usage <- foreach (i = row_indexes, .combine = "rbind") %do% {
    log_i <- log %>% 
      slice(i:(i + 16)) 
    
    # get cpus
    cpus <- log_i %>% 
      filter(str_detect(raw_text, "Cpus")) %>% 
      pull(raw_text)
    
    cpus <- str_split(cpus, " ") %>% 
      unlist() %>% 
      enframe(name = NULL, value = "split_text") %>% 
      filter(split_text != "\t" & split_text != "") %>% 
      unlist(use.names = FALSE)
    
    cpus_usage <- cpus[3]
    cpus_requested <- cpus[4]
    cpus_allocated <- cpus[5]
    
    # get disk
    disk <- log_i %>% 
      filter(str_detect(raw_text, "Disk")) %>% 
      pull(raw_text)
    
    disk <- str_split(disk, " ") %>% 
      unlist() %>% 
      enframe(name = NULL, value = "split_text") %>% 
      filter(split_text != "\t" & split_text != "") %>% 
      unlist(use.names = FALSE)
    
    disk_usage <- disk[4]
    disk_requested <- disk[5]
    disk_allocated <- disk[6]
    
    # get memory
    memory <- log_i %>% 
      filter(str_detect(raw_text, "Memory")) %>% 
      pull(raw_text)
    
    memory <- str_split(memory, " ") %>% 
      unlist() %>% 
      enframe(name = NULL, value = "split_text") %>% 
      filter(split_text != "\t" & split_text != "") %>% 
      unlist(use.names = FALSE)
    
    memory_usage <- memory[4]
    memory_requested <- memory[5]
    memory_allocated <- memory[6]
    
    # get job num
    job_num <- log_i %>% 
      slice(1) %>% 
      mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) %>% 
      pull(job_num)
    
    # create tibble
    tibble_i <- tibble(job_num, cpus_usage, cpus_requested, cpus_allocated, 
                       disk_usage, disk_requested, disk_allocated, 
                       memory_usage, memory_requested, memory_allocated) %>% 
      mutate(across(cpus_usage:memory_allocated, ~ as.numeric(.x)))
  }
  
  # merge into final log
  final_log <- final_log %>% 
    full_join(usage, by = "job_num") 
  
  # merge with jobs file
  final_log <- jobs_file %>% 
    left_join(final_log, by = "job_num")
  
  return(final_log)
}