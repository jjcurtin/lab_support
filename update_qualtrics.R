update_qualtrics <- function(survey_id, 
                             survey_type,
                             api_token,
                             root_url = "https://uwmadison.co1.qualtrics.com") {
  # update_qualtrics() allows you to update an existing qualtrics survey
  # NOTE:  This function is a simplified version of code
  # from Jasper Ginn's qualtRics package.  We use this because we had some problems
  # with the implementation of some of the more complex
  # aspects of his code that were unnecessary for our usage.
  
  # format input variables for API use
  if (survey_type == "daily") {
    template_id <- "SV_bdpR4B7K7Bc7nHE"
    template <- jsonlite::read_json("S:/optimize/administration/qualtrics/6_ema_opt_template.qsf")
    
  } else if (survey_type == "bimonthly") {
    template_id <- "SV_3w3BqdZl0fPfMa2"

  } else if (survey_type == "feedback") {
    template_id <- "SV_084MbCN54RfNrrU"
    template <- jsonlite::read_json("S:/optimize/administration/qualtrics/7_feedback_opt_template.qsf")
  }

  # add endpoint to root_url
  root_url <- str_c(root_url,
                    ifelse(substr(root_url, nchar(root_url),
                                  nchar(root_url)) == "/",
                           str_c("API/v3/surveys/", template_id),
                           str_c("/API/v3/surveys/", template_id)))
  

  
  # Create raw JSON payload
  payload <- str_c("{\n  \"SurveyName\": \"", this_survey, "\",\n  \"Language\": \"EN\",\n  \"ProjectCategory\": \"CORE\"\n}")
  
  
  payload <- "{\n  \"name\": \"string\",\n  \"isActive\": true,\n  \"expiration\": {\n    \"startDate\": \"2019-08-24T14:15:22Z\",\n    \"endDate\": \"2019-08-24T14:15:22Z\"\n  },\n  \"ownerId\": \"string\"\n}"
  
  encode <- "json"
  
  response <- VERB("PUT", url, body = payload, add_headers('X-API-TOKEN' = ''), content_type("application/json"), accept("application/json"), encode = encode)
  
  content(response, "text")
  
  
  
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
