update_qualtrics <- function(survey_id, 
                             survey_type,
                             api_token,
                             template_path,
                             root_url = "https://uwmadison.co1.qualtrics.com") {
  # update_qualtrics() allows you to update an existing qualtrics survey
  # NOTE:  This function is a simplified version of code
  # from Jasper Ginn's qualtRics package.  We use this because we had some problems
  # with the implementation of some of the more complex
  # aspects of his code that were unnecessary for our usage.
  # 
  # # format input variables for API use
  # if (survey_type == "daily") {
  #   template_id <- "SV_bdpR4B7K7Bc7nHE"
  #   # template <- jsonlite::read_json("S:/optimize/administration/qualtrics/6_ema_opt_template.qsf")
  #   
  # } else if (survey_type == "bimonthly") {
  #   template_id <- "SV_3w3BqdZl0fPfMa2"
  # 
  # } else if (survey_type == "feedback") {
  #   template_id <- "SV_084MbCN54RfNrrU"
  #   # template <- jsonlite::read_json("S:/optimize/administration/qualtrics/7_feedback_opt_template.qsf")
  # }

  library(httr)

  
  
  # add endpoint to root_url
  root_url <- str_c(root_url,
                    ifelse(substr(root_url, nchar(root_url),
                                  nchar(root_url)) == "/",
                           str_c("API/v3/surveys/", survey_id, "/import"),
                           str_c("/API/v3/surveys/", survey_id, "/import")))
  

  
  # # Create raw JSON payload
  template_path <- file.path(path_data_raw, sub, "created_surveys", str_c("ema_", today(), ".qsf"))
  
  # Construct headers
  headers = constructHeader(api_token)
  # 
  # encode <- "json"
  # 
  # response <- VERB("PUT", url, body = payload, add_headers('X-API-TOKEN' = ''), content_type("application/json"), accept("application/json"), encode = encode)
  # 
  # content(response, "text")
  # 
  # 
  
  # POST request for download
  result <- qualtrics_api_request(api_token, "PUT", URL = root_url, body = payload)
  
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



overwrite_qualtrics_survey <- function(
    api_token,
    datacenter,
    survey_id,
    qsf_path,
    deactivate_before = TRUE,
    reactivate_after = FALSE,
    verbose = TRUE) {
  # ---- Dependencies ----
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'httr' and 'jsonlite' are required.")
  }
  
  library(httr)
  library(jsonlite)
  
  # ---- Input validation ----
  if (!file.exists(qsf_path)) {
    stop("QSF file does not exist: ", qsf_path)
  }
  
  base_url <- sprintf(
    "https://%s.qualtrics.com/API/v3",
    datacenter
  )
  
  survey_url <- sprintf(
    "%s/surveys/%s",
    base_url,
    survey_id
  )
  
  import_url <- sprintf(
    "%s/import",
    survey_url
  )
  
  headers <- add_headers(
    "X-API-TOKEN" = api_token
  )
  
  # ---- Helper: deactivate/reactivate survey ----
  set_active_state <- function(is_active) {
    PUT(
      url = survey_url,
      headers,
      body = toJSON(list(isActive = is_active), auto_unbox = TRUE),
      encode = "json"
    )
  }
  
  # ---- Deactivate survey (recommended) ----
  if (deactivate_before) {
    if (verbose) message("Deactivating survey...")
    r <- set_active_state(FALSE)
    if (http_error(r)) {
      stop(
        "Failed to deactivate survey:\n",
        content(r, as = "text", encoding = "UTF-8")
      )
    }
  }
  
  # ---- Import (overwrite) QSF ----
  
  if (verbose) message("Importing QSF and overwriting survey...")
  
  response <- POST(
    url = import_url,
    headers,
    body = list(
      file = upload_file(template_path, type = "application/json"),
      overwrite = "true"
    ),
    encode = "multipart"
  )
  
  response_text <- content(response, as = "text", encoding = "UTF-8")
  
  if (http_error(response)) {
    stop(
      "Survey import failed (HTTP ",
      status_code(response),
      "):\n",
      response_text
    )
  }
  
  result <- fromJSON(response_text)
  
  # ---- Reactivate survey (optional) ----
  if (reactivate_after) {
    if (verbose) message("Reactivating survey...")
    r <- set_active_state(TRUE)
    if (http_error(r)) {
      warning(
        "Survey was imported, but reactivation failed:\n",
        content(r, as = "text", encoding = "UTF-8")
      )
    }
  }
  
  if (verbose) message("Survey overwrite completed successfully.")
  
  invisible(result)
}

  
  


