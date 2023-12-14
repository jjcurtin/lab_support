# Formats file path for OS

format_path <- function(the_path){

  switch (Sys.info()[['sysname']],
          # PC paths
          Windows = {the_path <- stringr::str_c("P:/", the_path)},
          # IOS paths
          Darwin = {the_path <- stringr::str_c("/Volumes/private/", the_path)},
          # Linux paths
          Linux = {the_path <- stringr::str_c("~/mnt/private/", the_path)}
  )
  return(the_path)
}
