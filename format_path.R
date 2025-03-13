# Formats file path for OS

format_path <- function(the_path, resource = "standard"){

  # standard is standard research drive (S drive for windows)
  # restricted is restricted research drive (R drive for windows)
  
  if(!(resource == "standard" | resource == "restricted" | resource == "private")){
    stop("Error: resource must be either 'standard', 'restricted', or 'private'")
  }
  
  switch (Sys.info()[['sysname']],
          
          # PC paths
          Windows = {the_path <- dplyr::if_else(resource == "standard", 
                                                stringr::str_c("P:/", the_path),
                                                stringr::str_c("R:/", the_path))},        
  
          # IOS paths
          Darwin = {the_path <- dplyr::if_else(resource == "standard",
                                               stringr::str_c("/Volumes/standard/", the_path),
                                               stringr::str_c("/Volumes/restricted/", the_path))},
          
          # Linux paths
          Linux = {the_path <- dplyr::if_else(resource == "standard",
                                              stringr::str_c("~/mnt/standard/", the_path),
                                              stringr::str_c("~/mnt/restricted/", the_path))},
  )
  return(the_path)
  
}
