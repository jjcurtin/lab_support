get_qualtrics_surveys <- function(api_token,
                             root_url = "https://uwmadison.co1.qualtrics.com",
                             encode = "json") {
  # get a list of qualtrics surveys in our account. will be used to check if a 
  # personalized survey has already been made for a participant

  
  # add endpoint to root_url
  root_url <- str_c(root_url,
                    ifelse(substr(root_url, nchar(root_url),
                                  nchar(root_url)) == "/",
                           "API/v3/surveys/",
                           "/API/v3/surveys/"))

    
  #construct headers
    this_header  <-  c(
      "X-API-TOKEN" = api_token,
      "Content-Type" = "application/octet-stream",
      "Accept" = "application/json",
      "accept-encoding" = "gzip, deflate"
    )

    
  # Send request to qualtrics API (httr functions)
  res <-  httr::VERB("GET",
               url = root_url,
               add_headers(this_header),
               body = NULL)
  
  results <- content(res)$result$elements |>
    enframe()
 
  
 surveylist <-  results$value |> 
   enframe() |> 
   unnest_wider(value, names_sep = "_") |> 
   select(value_id, value_name, creation_date = value_creationDate)
  
 # it can only get 100 surveys at a time, so to get them all we'll need to run a loop based on what next page offset is returned
 next_offset <- content(res)$result$nextPage |> str_extract("\\d{3}")
 
 while (length(next_offset) != 0) {
   
  print(str_c("looping for offset: ", next_offset)) 
  temp_res <-  httr::VERB("GET",
                         url = str_c(root_url, "?offset=", next_offset),
                         add_headers(this_header),
                         body = NULL)
   
  temp_results <- content(temp_res)$result$elements |>
     enframe()
   
   temp_list <-  temp_results$value |> 
     enframe() |> 
     unnest_wider(value, names_sep = "_") |> 
     select(value_id, value_name, creation_date = value_creationDate)
   
   surveylist <- rbind(temp_list, surveylist)
  
   next_offset <- content(temp_res)$result$nextPage |> str_extract("\\d{3}")
   
    
 }
 
  surveylist <- surveylist |> select(survey_id = value_id, survey_name = value_name, creation_date)
 
   return(surveylist)
}
