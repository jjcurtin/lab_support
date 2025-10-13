# Formats file path for OS

format_path <- function(the_path, resource = "standard"){

  # standard is standard research drive (S drive for windows)
  # restricted is restricted research drive (R drive for windows)
  
  os <- Sys.info()[['sysname']]
  
  if(!(resource == "standard" | resource == "restricted")){
    stop("Error: resource must be either 'standard' or 'restricted'")
  }
  if(resource == "restricted" &&  os == "Darwin"){
    stop("Error: resource must be 'standard' for Mac OS")
  }
  
  switch (Sys.info()[['sysname']],
          
          # PC paths
          Windows = {the_path <- dplyr::if_else(resource == "standard", 
                                                stringr::str_c("S:/", the_path),
                                                stringr::str_c("R:/", the_path))},        
  
          # IOS path
          Darwin = {the_path <- stringr::str_c("/Volumes/studydata/", the_path)},
          
          # Linux paths
          Linux = {the_path <- dplyr::if_else(resource == "standard",
                                              stringr::str_c("~/mnt/standard/", the_path),
                                              stringr::str_c("~/mnt/restricted/", the_path))},
  )
  return(the_path)
}
