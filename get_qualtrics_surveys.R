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
    this_header = c(
      "X-API-TOKEN" = api_token,
      "Content-Type" = "application/octet-stream",
      "Accept" = "application/json",
      "accept-encoding" = "gzip, deflate"
    )

    
  # Send request to qualtrics API (httr functions)
  res = httr::VERB("GET",
               url = root_url,
               add_headers(this_header),
               body = NULL)
  
  next_page <- content(res)$result$next_page
    
  results <- content(res)$result$elements |>
    enframe()
 
  
 surveylist <-  results$value |> 
   enframe() |> 
   unnest_wider(value, names_sep = "_") |> 
   select(value_id, value_name, creation_date = value_creationDate)
  

  this_survey <- str_c(subid, survey_name)  
 
   return(id)
}
