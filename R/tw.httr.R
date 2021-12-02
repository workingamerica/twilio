# tw.httr: do error handling and JSON parsing -----------------------------

#' Twilio Spoke API: HTTP Request Response Handling
#'
#' @keywords twilio sms spoke api
#' @export 
#' 
#' @usage tw.httr(resp, simplifyDataFrame=FALSE, debug=FALSE)
#' 
#' @param resp A \code{httr::response()} object; returned by \code{httr::GET()}, \code{httr::DELETE()}, etc.
#' @param simplifyDataFrame Attempt to simplify data responses? Parameter used in \code{jsonlite::fromJSON()}.
#' @param debug Return original response without attempting interpretation.
#' 

tw.httr <- function(resp, simplifyDataFrame=FALSE, debug=FALSE) {
  #### basic WTF error:
  if( httr::http_type(resp) != "application/json" ){
    warning("Twilio API did not return JSON.")
    if(debug) return(resp)
  }
  #### auth error:
  else if( resp$status_code == 401 ) {
    ## resp$status or resp$status_code?
    warning("Error: Authentication error, check SID and Token.")
    if(debug) return(resp)
  }
  
  #### unsuccessful delete:
  else if( resp$request$method == "DELETE" & resp$status_code == 404 ) {
    message("Could not delete the object at ",resp$request$url," because it doesn't exist.")
    return(resp)
  }
  #### successful delete:
  else if( resp$request$method == "DELETE" & resp$status_code == 204 ) {
    message("Successfully deleted the object at ",resp$request$url)
    return(resp)
  }
  
  #### misc other errors:
  else if( !(resp$status_code %in% c(200,201,204)) ) {
    # 200 = successful query
    # 201 = successful create
    # 204 = successful delete
    if(debug) {
      warning("Twilio API call not successful.")
      return(resp)
    }
    else {
      resp <- jsonlite::fromJSON(
        httr::content(resp, "text", encoding = "UTF-8"), 
        simplifyVector = simplifyDataFrame,
        simplifyDataFrame = simplifyDataFrame
      )
      
      warning(
        "Twilio error ",resp$status,"\n",
        resp$detail,"\n",
        resp$message,"\n",
        "More information: ",resp$more_info
      )
    }
    return(resp)
  }
  #### misc other successes
  else {
    ## parse the response
    parsed <- jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"), 
      simplifyVector = simplifyDataFrame,
      simplifyDataFrame = simplifyDataFrame
    )
    
    return(parsed)
  }
}