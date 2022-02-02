# find a # based on geography/area code --------------------------

#' Twilio Spoke API: Find Phone Number to Purchase
#' 
#' @keywords twilio sms spoke api
#' @export tw.available_phone_local
#' 
#' @usage tw.available_phone_local(
#'   sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
#'   key = Sys.getenv('TWILIO_KEY'),
#'   secret = Sys.getenv('TWILIO_SECRET'),
#'   page_size = 1,
#'   area_code = NA,
#'   near_number = NA,
#'   distance = ifelse(is.na(near_number),NA,25),
#'   in_postal_code = NA,
#'   in_locality = NA,
#'   verbose = FALSE,
#'   plyr = FALSE
#' )
#' 
#' @param sid twilio credentials: Account SID.
#' @param key twilio credentials: Account user key.
#' @param secret twilio credentials: User secret. Don't store this in scripts!!!
#' @param area_code The area code of the phone numbers to read. Applies to only phone numbers in the US and Canada.
#' @param page_size Number of results to return. Default is 1.
#' @param near_number Given a phone number, find a geographically close number within distance miles. Distance defaults to 25 miles. Applies to only phone numbers in the US and Canada.
#' @param distance The search radius, in miles, for a near_ query. Can be up to 500 and the default is 25. Applies to only phone numbers in the US and Canada.
#' @param in_postal_code Limit results to a particular postal code. Given a phone number, search within the same postal code as that number. Applies to only phone numbers in the US and Canada.
#' @param in_locality Limit results to a particular locality or city. Given a phone number, search within the same Locality as that number.
#' @param verbose Print detailed progress.
#' @param plyr Removes a column that plyr can't deal with, to allow parallelization
#' 
#' @section Returns:
#' ? 
#' 
#' @section Details:
#' ?
#' 
#' @examples
#'
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' x <- tw.available_phone_local(sid,key,secret, area_code = 303, page_size=10)
#' 
#' x <- tw.available_phone_local(sid,key,secret, near_number = 7205559860, distance = 100, page_size=10)
#' 
#' (x <- tw.available_phone_local(sid,key,secret, in_postal_code = 81611, page_size=10))
#' }

tw.available_phone_local <- function(
  sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
  key = Sys.getenv('TWILIO_KEY'),
  secret = Sys.getenv('TWILIO_SECRET'),
  page_size = 1,
  area_code = NA,
  near_number = NA,
  distance = ifelse(is.na(near_number),NA,25),
  in_postal_code = NA,
  in_locality = NA,
  verbose = FALSE,
  plyr = FALSE
) {
  ## check for/create auth token:
  auth <- tw.auth(sid=key,token=secret)
  if(is.na(sid)) sid <- Sys.getenv('TWILIO_ACCOUNT_SID')
  
  if( !is.na(area_code) & nchar(area_code)!=3 ) {
    stop("You must provide a US or Canada area code (3 digits).", call.=F)
  }
  
  # curl -X GET 'https://api.twilio.com/2010-04-01/Accounts/ACXX..XX/AvailablePhoneNumbers/US/Local.json?AreaCode=510&PageSize=20'
  u <- paste0(
    'https://api.twilio.com/2010-04-01/Accounts/',sid,
    '/AvailablePhoneNumbers/US/Local.json'
  )
  ## query vars for end of URL.
  qvars <- list()
  # looks like httr::POST() already does urlencode, so no need to duplicate.
  # only interested in SMS/MMS-capable phones:
  qvars$SmsEnabled <- 'true'
  qvars$MmsEnabled <- 'true'
  qvars$PageSize <- page_size
  if(!is.na(area_code)) { qvars$AreaCode <- area_code }
  if(!is.na(near_number)) { qvars$NearNumber <- near_number }
  if(!is.na(distance)) { qvars$Distance <- distance }
  if(!is.na(in_postal_code)) { qvars$InPostalCode <- in_postal_code }
  if(!is.na(in_locality)) { qvars$InLocality <- in_locality }
  if(verbose) print(qvars)
  
  ## make request:
  resp <- httr::GET( url = u, query = qvars, auth )#, httr::verbose())
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp, simplifyDataFrame=TRUE)
  if(resp$status_code!=200) {
    return(data.table::as.data.table(parsed))
  } else {
    # because simplifyDataFrame, don't need to mess around w/ plyr
    parsed <- parsed$available_phone_numbers
    if( length(parsed)==0 ) {
      warning("No matching phones were found.", call.=F)
    }
    if(plyr) {
      parsed$capabilities <- NULL # this messes up the returned data frame.
    }
    return(parsed)
  }
}