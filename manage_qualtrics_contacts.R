manage_qualtrics_contacts <- function(api_token,
                             root_url = "https://uwmadison.co1.qualtrics.com/API/v3/directories/",
                             pool = "optimize",
                             encode = "json",
                             action, # list, create, update, or delete
                             payload, # participant contact info for create or update
                             contact_id) { # participant contact ID for update or delete
  # Functions to List, Create, Update, or Delete directory contacts which 
  # allows us to manage participants and their embedded data for EMA

  # API Toolkit: https://api.qualtrics.com/83b3836dfc351-mailing-list-contacts

  
  # choose contact poolID & listID
  if (pool == "optimize") {
    pool_id <- "POOL_1hGT2QgkMbycmHK"
    list_id <- "CG_3Grm1bKdFpBoMlO"
  }
  
## works
  #list the contacts in our contact list
  if (action == "list") {
    contact_url <- str_c(root_url, 
                         pool_id, 
                         "/mailinglists/", 
                         list_id, 
                         "/contacts")
    
    all_contacts <- list()
    next_page_url <- contact_url
    
    repeat {
      req <- httr2::request(next_page_url) |>
        httr2::req_headers(
          "X-API-TOKEN" = api_token,
          "Content-Type" = "application/json"
        ) |>
        httr2::req_error(is_error = \(resp) httr2::resp_status(resp) >= 400)
      
      response <- httr2::req_perform(req)
      parsed <- httr2::resp_body_json(response, simplifyVector = TRUE)
      contacts_page <- parsed$result$elements
      
      # Unnest embeddedData column if present
      if (!is.null(contacts_page$embeddedData) && 
          is.data.frame(contacts_page$embeddedData)) {
        contacts_page <- cbind(
          contacts_page[, setdiff(names(contacts_page), "embeddedData")],
          contacts_page$embeddedData
        )
      }
      
      # Collect pages as list, not rbind in a loop
      all_contacts <- c(all_contacts, list(contacts_page))
      
      next_page_url <- parsed$result$nextPage
      if (is.null(next_page_url)) break
    }
    
    # Bind all pages at once at the end
    return(dplyr::bind_rows(all_contacts) |> dplyr::as_tibble())
  }
  
  
 # works
 if (action == "create") {
   create_url <- str_c(root_url, 
                       pool_id, 
                       "/mailinglists/", 
                       list_id, 
                       "/contacts")
   
   # Remove any NULL optional fields before sending
   payload <- Filter(Negate(is.null), payload)
   
   
   req <- httr2::request(contact_url) |>
     httr2::req_headers(
       "X-API-TOKEN" = api_token,
       "Content-Type" = "application/json"
     ) |>
     httr2::req_method("POST") |>
     httr2::req_body_json(payload) |>
     httr2::req_error(is_error = \(resp) httr2::resp_status(resp) >= 400)
   
   response <- httr2::req_perform(req)
   parsed <- httr2::resp_body_json(response, simplifyVector = TRUE)
   
   # Returns the new contact's ID
   return(parsed$result$id)
 }
  
  if (action == "update") {
    update_url <- str_c(root_url, 
                        pool_id, 
                        "/mailinglists/", 
                        list_id, 
                        "/contacts/", 
                        contact_id)
    
   
    payload <- clean_body(payload)
    
    req <- httr2::request(update_url) |>
      httr2::req_headers(
        "X-API-TOKEN" = api_token,
        "Content-Type" = "application/json"
      ) |>
      httr2::req_method("PUT") |>
      httr2::req_body_json(payload) |>
      httr2::req_error(is_error = \(resp) httr2::resp_status(resp) >= 400)
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      return("contact updated")
    } else {
      return("update failed")
    }
  }
  
  if (action == "delete") {
    delete_url <- str_c(root_url, 
                        pool_id, 
                        "/mailinglists/", 
                        list_id, 
                        "/contacts/", 
                        contact_id)
    
    req <- httr2::request(delete_url) |>
      httr2::req_headers(
        "X-API-TOKEN" = api_token,
        "Content-Type" = "application/json"
      ) |>
      httr2::req_method("DELETE") |>
      httr2::req_error(is_error = \(resp) httr2::resp_status(resp) >= 400)
    
    response <- httr2::req_perform(req)
    parsed <- httr2::resp_body_json(response, simplifyVector = TRUE)
    
    if (parsed$meta$httpStatus == "200 - OK") {
      return("contact deleted")
    } else {
      return("deletion failed")
    }
  }  
  
  
  
}
