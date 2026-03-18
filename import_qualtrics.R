import_qualtrics <- function(subid,
                             survey_name,
                             api_token,
                             root_url = "https://uwmadison.co1.qualtrics.com",
                             encode = "json") {
  # import_qualtrics() creates a new qualtrics survey based on a template qsf file. It returns the surveyID
  # of the newly created survey. It is intended for use with update_qualtrics()
  # which together build a new survey for a study participant based on a template

  # NOTE: based on John Curtins simplification of Jasper Ginn's qualtRics package.  
  # We use this because we had some problems with the implementation of some of 
  # the more complex aspects of his code that were unnecessary for our usage.
 
  TEST_TEMPLATE = "SV_bdpR4B7K7Bc7nHE"
  
  # format input variables for API use
  if (survey_name == "daily") {
    survey_name <- "Daily Survey"
    
  } else if (survey_name == "bimonthly") {
    survey_name <- "Bimonthly Survey"
  }
  
  this_survey <- str_c(subid, " ", survey_name)  
  
  
  # add endpoint to root_url
  root_url <- str_c(root_url,
                    ifelse(substr(root_url, nchar(root_url),
                                  nchar(root_url)) == "/",
                           "API/v3/survey-definitions/",
                           "/API/v3/survey-definitions/"))

  this_survey <- str_c(subid, survey_name)  
  
  
  
  
  
  ----
  
  payload <- "{\n  \"projectName\": \"string\"\n}"
  
  encode <- "json"
  
  response <- VERB("POST", url, body = payload, add_headers('X-COPY-SOURCE' = '', 'X-COPY-DESTINATION-OWNER' = '', 'X-API-TOKEN' = ''), content_type("application/json"), accept("application/json"), encode = encode)
  
  content(response, "text")
  
  -----
  
  
  
  
  
  
  # Create raw JSON payload
  payload <- str_c("{\n  \"SurveyName\": \"", this_survey, "\",\n  \"Language\": \"EN\",\n  \"ProjectCategory\": \"CORE\"\n}")
  

  # POST request for download
  result <- qualtrics_api_request(api_token, "POST", URL = root_url, body = payload)
  
  # Get ID
  if (is.null(result$result)) {
    if (is.null(result$result$SurveyID)) {
      stop("Something went wrong. Please re-run your query.")
    } else {
      id <- result$result$SurveyID
    }
  } else {
    id <- result$result$SurveyID
  } # NOTE This is not fail safe because ID can still be NULL
  
   return(id)
}
