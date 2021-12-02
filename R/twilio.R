# TODO --------------------------------------------------------------------

# Do the following operations:
# 
# x. List details for any given phone/message service
# x. Delete/drop a phone from all message services and the account
# x. Purchase phones by area code
# x. Add and configure a message service
# x. Move a phone from one message service to another
# x. add to a messaging service
# 
# 0. List owned phones and their message services
# 1b. change friendly name of a phone


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



# list phones -------------------------------------------------------------

#' Twilio Spoke API: List Owned Phones
#' 
#' @keywords twilio sms spoke api
#' @export tw.incoming_phone_numbers.list
#' 
#' @usage tw.incoming_phone_numbers.list(
#'   sid = NA, token = NA, 
#'   n = 20, 
#'   phone_number = NA, 
#'   friendly_name = NA, 
#'   verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param n number of records to return, for API calls that return more than one record.
#' @param phone_number The phone numbers of the IncomingPhoneNumber resources to read. You can specify partial numbers and use '*' as a wildcard for any digit.
#' @param friendly_name Provide full (not partial :P) friendly name to match only a subset of phones.
#' @param verbose Print detailed progress.
#' 
#' @section Returns:
#' A \code{data.table}. NOTE: twilio API allows max 1000 to be returned.
#' 
#' @section Details:
#' \code{tw.incoming_phone_numbers.list()} gets a list of owned phone numbers.
#' 
#' \code{tw.incoming_phone_numbers.fetch()} gets the details of a specific owned phone number.
#' 
#' \code{tw.incoming_phone_numbers.delete()} deletes a specific owned phone number from your twilio account. This removes it from your bill.
#' 
#' \code{tw.messages.list()} gets the most recent \code{n} messages. You can specify variables to narrow the query.
#' 
#' @examples
#' 
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' tw.incoming_phone_numbers.list(sid, token, n=20)
#' }

tw.incoming_phone_numbers.list <- function(
	sid = NA, token = NA, 
	n = 20, 
	phone_number = NA,
	friendly_name = NA, 
	verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  u <- paste0(
    'https://api.twilio.com/2010-04-01/Accounts/',sid,
    '/IncomingPhoneNumbers.json?'
  )
  qvars <- list(PageSize=n)
  if(!is.na(phone_number)) {qvars$PhoneNumber = phone_number}
  if(!is.na(friendly_name)) {qvars$FriendlyName = friendly_name}
  resp <- httr::GET( url = u, query = qvars, auth )
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp, simplifyDataFrame = TRUE)
  
  # unlist each element, then combine into a data frame:
  parsed <- parsed$incoming_phone_numbers
  parsed <- data.table::as.data.table(parsed)
  
  return(parsed)
}



# fetch details on one phone ----------------------------------------------

#' Twilio Spoke API: Fetch Details on One Phone
#' 
#' @keywords twilio sms spoke api
#' @export tw.incoming_phone_numbers.fetch
#' 
#' @usage tw.incoming_phone_numbers.fetch(sid = NA, token = NA, phone_sid, verbose = FALSE)
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param phone_sid twilio phone record SID (format 'PNXXXX...XXXX')
#' @param verbose Print detailed progress.
#' 
#' @section Details:
#' ?
#' 
#' @examples
#' 
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' tw.incoming_phone_numbers.fetch(
#'   sid, token, 
#'   phone_sid='PN9e35ee12e6e9aaf3fc5dc39a5025df3c'
#' )
#' }
#' 

tw.incoming_phone_numbers.fetch <- function(sid = NA, token = NA, phone_sid, verbose = FALSE) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  # curl -X GET 'https://api.twilio.com/2010-04-01/Accounts/ACXX..XX/IncomingPhoneNumbers/PNXX..XX.json'
  u <- paste0(
    'https://api.twilio.com/2010-04-01/Accounts/',sid,
    '/IncomingPhoneNumbers/',phone_sid,'.json'
  )
  resp <- httr::GET( url = u, auth )
  if(verbose) print(resp)
  
  parsed <- tw.httr(resp)
  
  # unlist the returned element, then make into a data frame:
  parsed <- unlist(parsed)
  parsed <- t(parsed)
  parsed <- data.table::as.data.table(parsed)
  
  return(parsed)
}


# delete a phone -------------------------------------------------------------

#' Twilio Spoke API: Delete a phone from your account
#' 
#' @keywords twilio sms spoke api
#' @export tw.incoming_phone_numbers.delete
#' 
#' @usage tw.incoming_phone_numbers.delete(sid = NA, token = NA, phone_sid, verbose = FALSE)
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param phone_sid twilio phone record SID (format 'PNXXX...XXX')
#' @param verbose Print detailed progress.
#' 
#' @section Details:
#' ?
#' 
#' @examples
#' 
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' tw.incoming_phone_numbers.delete(sid,token,'PN41fb306bf5c87631fb85ccabd3905843')
#' }

tw.incoming_phone_numbers.delete <- function(sid = NA, token = NA, phone_sid, verbose = FALSE) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if(is.na(phone_sid)) {
    stop("Must suppy the SID of the phone to delete.", call.=F)
  }
  
  # curl -X DELETE https://api.twilio.com/2010-04-01/Accounts/ACXX..XX/IncomingPhoneNumbers/PNXX..XX.json
  u <- paste0(
    'https://api.twilio.com/2010-04-01/Accounts/',sid,
    '/IncomingPhoneNumbers/',phone_sid,'.json'
  )
  resp <- httr::DELETE( url = u, auth )
  ## handle response & any errors:
  resp <- tw.httr(resp)
  if(verbose) print(resp)
  return(resp)
}

# purchase/create a phone ---------------------------------------------------------

#' Twilio Spoke API: Purchase Phone Number
#' 
#' @keywords twilio sms spoke api
#' @export tw.incoming_phone_numbers.create
#' 
#' @usage tw.incoming_phone_numbers.create(
#'   sid = NA, token = NA, 
#'   area_code = NA, 
#'   phone_number = NA,
#'   voice_url = '', 
#'   friendly_name = NA,
#'   verbose = FALSE )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param area_code The desired area code for your new incoming phone number. Can be any three-digit, US or Canada area code. We will provision an available phone number within this area code for you. You must provide an area_code or a phone_number. (US and Canada only).
#' @param phone_number The phone number to purchase specified in E.164 format. E.164 phone numbers consist of a + followed by the country code and subscriber number without punctuation characters. For example, +14155551234.
#' @param voice_url The URL that we should call to answer a call to the new phone number.
#' @param friendly_name A descriptive string that you created to describe the new phone number. It can be up to 64 characters long. By default, this is a formatted version of the new phone number.
#' @param verbose Print detailed progress.
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
#' x <- tw.incoming_phone_numbers.create(
#'   sid, token,
#'   area_code = 480,
#'   voice_url = '',
#'   friendly_name = 'Working America (AZ 480)'
#' )
#' httr::content(x, "text", encoding = "UTF-8")
#' }

tw.incoming_phone_numbers.create <- function(
  sid = NA, token = NA, 
  area_code = NA,
  phone_number = NA,
  voice_url = '',
  friendly_name = NA,
  verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if(is.na(phone_number)==is.na(area_code)) {
    stop("You must specify exactly one of area_code and phone_number.", call.=F)
  }
  if( !is.na(area_code) & nchar(area_code)!=3 ) {
    stop("You must provide a US or Canada area code (3 digits).", call.=F)
  }
  
  u <- paste0(
    'https://api.twilio.com/2010-04-01/Accounts/',sid,
    '/IncomingPhoneNumbers.json'
  )
  ## query vars for end of URL.
  qvars <- list()
  # looks like httr::POST() already does urlencode, so no need to duplicate.
  if(!is.na(area_code)) { qvars$AreaCode <- area_code }
  if(!is.na(phone_number)) { qvars$PhoneNumber <- phone_number }
  if(!is.na(voice_url)) { qvars$VoiceUrl <- voice_url }
  if(!is.na(friendly_name)) { qvars$FriendlyName <- friendly_name }
  if(verbose) print(qvars)
  
  ## make request:
  ## via Twilio helpdesk, POST only takes form encoding, not JSON! wild.
  resp <- httr::POST( url = u, body = qvars, encode = "form" , auth )#, httr::verbose())
  if(verbose) print(resp)
  ## handle errors and etc:
  parsed <- tw.httr(resp)
  
  parsed <- unlist(parsed,recursive=T)
  parsed <- t(parsed)
  parsed <- data.table::as.data.table(parsed)
  
  return(parsed)
}

# find a # based on geography/area code --------------------------

#' Twilio Spoke API: Find Phone Number to Purchase
#' 
#' @keywords twilio sms spoke api
#' @export tw.available_phone_local
#' 
#' @usage tw.available_phone_local(
#'   sid = NA,
#'   token = NA,
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
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
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
#' x <- tw.available_phone_local(sid, token, area_code = 303, page_size=10)
#' 
#' x <- tw.available_phone_local(sid, token, near_number = 7209339860, distance = 100, page_size=10)
#' 
#' (x <- tw.available_phone_local(sid, token, in_postal_code = 81611, page_size=10))
#' }

tw.available_phone_local <- function(
  sid = NA,  token = NA,
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
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
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


# latest messages ---------------------------------------------------------

#' Twilio Spoke API: Latest Messages
#' 
#' @keywords twilio sms spoke api
#' @export tw.messages.list
#' 
#' @usage tw.messages.list(
#'   sid = NA, token = NA, 
#'   from = NA, 
#'   to = NA, 
#'   datesent = NA, 
#'   n = 20, 
#'   verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param n number of records to return, for API calls that return more than one record.
#' @param from number a message was sent from
#' @param to number a message was sent to
#' @param datesent date a message was sent, 'YYYY-MM-DD' format
#' @param verbose Print detailed progress.
#' 
#' @section Details:
#' ?
#' 
#' @examples
#' 
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' tw.messages.list(sid, token, n=20, from='19202893705')
#' }

tw.messages.list <- function(
  sid = NA, token = NA,
  from = NA,
  to = NA,
  datesent = NA,
  n = 20,
  verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  
  # curl -X GET 'https://api.twilio.com/2010-04-01/Accounts/ACXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  # /Messages.json?
  # DateSent=2016-08-31T00%3A00%3A00Z
  # &From=%2B15017122661
  # &To=%2B15558675310
  # &PageSize=20'
  
  u <- paste0(
    'https://api.twilio.com/2010-04-01/Accounts/',sid,'/Messages.json'
  )
  ## query vars for end of URL.
  qvars <- list()
  # looks like httr::POST() already does urlencode, so no need to duplicate.
  if(!is.na(datesent)) { qvars$DateSent <- datesent }
  if(!is.na(from)) { qvars$From <- from }
  if(!is.na(to)) { qvars$To <- to }
  if(!is.na(n)) { qvars$PageSize <- n }
  if(verbose) print(qvars)
  
  resp <- httr::GET( url = u, query = qvars, auth )
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp, simplifyDataFrame = TRUE)
  # print(summary(parsed))
  
  # save only $messages, and return as a data.table
  parsed <- data.table::as.data.table(parsed$messages)
  
  return(parsed)
}


# create a messaging service ----------------------------------------------

#' Twilio Spoke API: Create Messaging Service
#' 
#' @keywords twilio sms spoke api
#' @export tw.messaging_services.create
#' 
#' @usage tw.messaging_services.create(
#'   sid = NA,  token = NA,
#'   baseurl = 'https://wkamspoke.herokuapp.com',
#'   friendly_name = NA,
#'   area_code_geomatch = TRUE,
#'   verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param baseurl URL of your Spoke instance.
#' @param friendly_name Friendly name for the new service. Required.
#' @param area_code_geomatch Delivers message from US or Canadian Twilio phone number with a matching area code or overlay to your recipient.
#' @param verbose Print detailed progress.
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
#' # create a service
#' m <- tw.messaging_services.create(sid, token, friendly_name = 'quite friendly!')
#' # list all services
#' x <- tw.messaging_services.list(sid, token, n=20)
#' # delete a service
#' tw.messaging_services.delete(sid, token, m$sid)
#' }

tw.messaging_services.create <- function(
  sid = NA,  token = NA,
  baseurl = 'https://wkamspoke.herokuapp.com',
  friendly_name = NA,
  area_code_geomatch = TRUE,
  verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if( is.na(friendly_name) ) {
    stop("You must provide a friendly name for the service.", call.=F)
  }
  
  # curl -X POST https://messaging.twilio.com/v1/Services \
  # --data-urlencode "StatusCallback=http://requestb.in/1234abcd" \
  # --data-urlencode "FriendlyName=My First Service" \
  # -u ACXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX:your_auth_token
  u <- 'https://messaging.twilio.com/v1/Services'
  ## query vars for end of URL.
  qvars <- list()
  # looks like httr::POST() already does urlencode, so no need to duplicate.
  qvars$StatusCallback <- paste0(baseurl,'/twilio-message-report')
  qvars$FriendlyName <- friendly_name
  qvars$InboundRequestUrl <- paste0(baseurl,'/twilio')
  qvars$InboundMethod <- 'POST'
  qvars$FallbackUrl <- paste0(baseurl,'/twilio')
  qvars$FallbackMethod <- 'POST'
  qvars$StickySender <- TRUE
  qvars$AreaCodeGeomatch <- area_code_geomatch
  if(verbose) print(qvars)
  
  ## make request:
  ## via Twilio helpdesk, POST only takes form encoding, not JSON! wild.
  resp <- httr::POST( url = u, body = qvars, encode = "form" , auth )#, httr::verbose())
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp)
  
  parsed$links <- NULL # don't need it and it messes up the data.table
  parsed <- data.table::as.data.table ( parsed )
  
  return(parsed)
}

# list messaging services -------------------------------------------------

#' Twilio Spoke API: List Messaging Services
#' 
#' @keywords twilio sms spoke api
#' @export tw.messaging_services.list
#' 
#' @usage tw.messaging_services.list(sid = NA, token = NA, n = 20, alldata = FALSE, verbose = FALSE)
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param n number of records to return, for API calls that return more than one record.
#' @param alldata return allll the fields or just the marginally useful ones?
#' @param verbose Print detailed progress.
#' 
#' @section Details:
#' ?
#' 
#' @examples
#' 
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' # create a service
#' m <- tw.messaging_services.create(sid, token, friendly_name = 'quite friendly!')
#' # list all services
#' x <- tw.messaging_services.list(sid, token, n=20)
#' # delete a service
#' tw.messaging_services.delete(sid, token, m$sid)
#' }

tw.messaging_services.list <- function(sid = NA, token = NA, n = 20, alldata = FALSE, verbose = FALSE) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  # curl -X GET 'https://messaging.twilio.com/v1/Services?PageSize=20' ...
  u <- paste0(
    'https://messaging.twilio.com/v1/Services'
  )
  ## query vars for end of URL.
  qvars <- list(
    PageSize = n
  )
  
  resp <- httr::GET( url = u, query = qvars,  auth )
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp, simplifyDataFrame = TRUE)
  # print(summary(parsed))
  
  parsed <- data.table::as.data.table(parsed$services)
  # there's a lot of cruft, remove it by default:
  if(!alldata) {
    parsed <- dplyr::select(
      parsed, 
      "sid","friendly_name","date_created","date_updated",
      "area_code_geomatch","inbound_request_url"
    )
  }
  
  return(parsed)
}


# update an existing messaging service ------------------------------------

#' Twilio Spoke API: Update a Messaging Service
#' 
#' @keywords twilio sms spoke api
#' @export tw.messaging_services.update
#' 
#' @usage tw.messaging_services.update(
#'   sid = NA, token = NA, 
#'   service_sid = NA,
#'   baseurl = NA,
#'   friendly_name = NA,
#'   area_code_geomatch = TRUE,
#'   verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param service_sid twilio SID of messaging service to update
#' @param baseurl URL of your Spoke instance, e.g. 'https://wkamspoke.herokuapp.com'.
#' @param friendly_name Friendly name for the service.
#' @param area_code_geomatch Delivers message from US or Canadian Twilio phone number with a matching area code or overlay to your recipient.
#' @param verbose Print detailed progress.
#' 
#' @section Details:
#' ?
#' 
#' @examples
#' 
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' # create a service
#' m <- tw.messaging_services.create(sid, token, friendly_name = 'quite friendly!')
#' # update a service
#' tw.messaging_services.update(
#'   sid, token, 
#'   service_sid = m$sid,
#'   friendly_name = 'new friendly name',
#'   area_code_geomatch = FALSE
#' )
#' # list all services
#' x <- tw.messaging_services.list(sid, token, n=20)
#' # delete a service
#' tw.messaging_services.delete(sid, token, m$sid)
#' }


tw.messaging_services.update <- function(
  sid = NA, token = NA, 
  service_sid = NA,
  baseurl = NA,
  friendly_name = NA,
  area_code_geomatch = TRUE,
  verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if(is.na(service_sid)) {
    stop("Must provide a service SID to update.", call.=F)
  }
  
  u <- paste0('https://messaging.twilio.com/v1/Services/',service_sid)
  ## query vars for end of URL.
  qvars <- list()
  if(!is.na(baseurl)) {
    qvars$StatusCallback <- paste0(baseurl,'/twilio-message-report')
    qvars$InboundRequestUrl <- paste0(baseurl,'/twilio')
    qvars$FallbackUrl <- paste0(baseurl,'/twilio')
  }
  if(!is.na(friendly_name)) qvars$FriendlyName <- friendly_name
  if(!is.na(area_code_geomatch)) qvars$AreaCodeGeomatch <- area_code_geomatch
  
  ## make request:
  ## via Twilio helpdesk, POST only takes form encoding, not JSON! wild.
  resp <- httr::POST( url = u, body = qvars, encode = "form" , auth )#, httr::verbose())
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp)
  
  parsed$links <- NULL # don't need it and it messes up the data.table
  parsed <- data.table::as.data.table ( parsed )
  
  return(parsed)
}

# delete a messaging service ----------------------------------------------

#' Twilio Spoke API: Delete a Messaging Service
#' 
#' @keywords twilio sms spoke api
#' @export tw.messaging_services.delete
#' 
#' @usage tw.messaging_services.delete(
#'   sid = NA,  token = NA,
#'   service_sid = NA, verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param service_sid twilio SID of messaging service to delete
#' @param verbose Print detailed progress.
#' 
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
#' m <- tw.messaging_services.create(sid, token, friendly_name = 'quite friendly!')
#' tw.messaging_services.delete(sid, token, m$sid)
#' }


tw.messaging_services.delete <- function(
  sid = NA,  token = NA,
  service_sid = NA, verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if(is.na(service_sid)) {
    stop("Must suppy the SID of the messaging service to delete.", call.=F)
  }
  
  # curl -X DELETE https://messaging.twilio.com/v1/Services/MGXX..XX
  u <- paste0('https://messaging.twilio.com/v1/Services/',service_sid)

  ## make request:
  ## via Twilio helpdesk, it only takes form encoding, not JSON! wild.
  resp <- httr::DELETE( url = u, auth )#, httr::verbose())
  if(verbose) print(resp)
  ## handle response & any errors:
  resp <- tw.httr(resp)
  return(resp)
}


# add phone to messaging service ------------------------------------------

#' Twilio Spoke API: Add Phone to Messaging Service
#' 
#' @keywords twilio sms spoke api
#' @export tw.messaging_services.phone.add
#' 
#' @usage tw.messaging_services.phone.add(
#'   sid = NA,  token = NA,
#'   service_sid = NA,
#'   phone_sid = NA, verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param service_sid twilio SID of messaging service to add to
#' @param phone_sid twilio SID of phone to add
#' @param verbose Print detailed progress.
#' 
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
#' m <- tw.messaging_services.create(sid, token, friendly_name = 'quite friendly!')
#' tw.messaging_services.delete(sid, token, m$sid)
#' }


tw.messaging_services.phone.add <- function(
  sid = NA,  token = NA,
  service_sid = NA,
  phone_sid = NA,
  verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if(is.na(service_sid)) {
    stop("Must suppy the SID of the messaging service.", call.=F)
  }
  if(is.na(phone_sid)) {
    stop("Must suppy the SID of the phone.", call.=F)
  }
  
  # curl -X POST https://messaging.twilio.com/v1/Services/MGXX..XX/PhoneNumbers \
  # --data-urlencode "PhoneNumberSid=PNXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
  u <- paste0('https://messaging.twilio.com/v1/Services/',service_sid,'/PhoneNumbers')
  
  ## vars for POST.
  qvars <- list(PhoneNumberSid = phone_sid)
  
  ## make request:
  ## via Twilio helpdesk, POST only takes form encoding, not JSON! wild.
  resp <- httr::POST( url = u, body = qvars, encode = "form" , auth )#, httr::verbose())
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp, simplifyDataFrame = TRUE)

  parsed <- data.table::as.data.table( parsed )
  
  return(parsed)
}


# delete phone from message service ---------------------------------------

#' Twilio Spoke API: Delete Phone from Messaging Service
#' 
#' @keywords twilio sms spoke api
#' @export tw.messaging_services.phone.delete
#' 
#' @usage tw.messaging_services.phone.delete(
#'   sid = NA,  token = NA,
#'   service_sid = NA,
#'   phone_sid = NA,
#'   verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param service_sid twilio SID of messaging service to delete from
#' @param phone_sid twilio SID of phone to delete. Note that this does NOT remove the phone from your account, just from the message service.
#' @param verbose Print detailed progress.
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
#' m <- tw.messaging_services.create(sid, token, friendly_name = 'quite friendly!')
#' tw.messaging_services.delete(sid, token, m$sid)
#' }


tw.messaging_services.phone.delete <- function(
  sid = NA,  token = NA,
  service_sid = NA,
  phone_sid = NA,
  verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if(is.na(service_sid)) {
    stop("Must suppy the SID of the messaging service.", call.=F)
  }
  if(is.na(phone_sid)) {
    stop("Must suppy the SID of the phone.", call.=F)
  }
  
  # curl -X DELETE https://messaging.twilio.com/v1/Services/MGXX..XX/PhoneNumbers/PNXX..XX
  u <- paste0('https://messaging.twilio.com/v1/Services/',service_sid,
              '/PhoneNumbers/',phone_sid)
  
  ## make request:
  ## via Twilio helpdesk, POST only takes form encoding, not JSON! wild.
  resp <- httr::DELETE( url = u, auth )#, httr::verbose())
  if(verbose) print(resp)
  
  ## handle errors and etc:
  parsed <- tw.httr(resp)
  return(parsed)
}


# list phones in messaging service ----------------------------------------

#' Twilio Spoke API: List Phones In A Messaging Service
#' 
#' @keywords twilio sms spoke api
#' @export tw.messaging_services.phone.list
#' 
#' @usage tw.messaging_services.phone.list(
#'   sid = NA,  token = NA,
#'   service_sid = NA,
#'   n = 400, verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param service_sid twilio SID of messaging service to delete
#' @param n number of phones to return---400 is the max in a messaging service.
#' @param verbose Print detailed progress.
#' 
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
#' m <- tw.messaging_services.create(sid, token, friendly_name = 'quite friendly!')
#' tw.messaging_services.delete(sid, token, m$sid)
#' }


tw.messaging_services.phone.list <- function(
  sid = NA,  token = NA,
  service_sid = NA,
  n = 400, verbose = FALSE
) {
  ## check for/create auth token:
  if( is.na(sid) | is.na(token) ) {
    stop("Please specify both SID and token.", call.=F)
  }
  auth <- httr::authenticate(sid,token)
  
  if(is.na(service_sid)) {
    stop("Must suppy the SID of the messaging service.", call.=F)
  }
  
  # curl -X GET 'https://messaging.twilio.com/v1/Services/MGXX..XX/PhoneNumbers?PageSize=20'
  u <- paste0('https://messaging.twilio.com/v1/Services/',service_sid,'/PhoneNumbers')
  
  ## vars for GET
  qvars <- list(PageSize = n)
  
  ## make request:
  ## via Twilio helpdesk, POST only takes form encoding, not JSON! wild.
  resp <- httr::GET( url = u, query = qvars, auth )#, httr::verbose())
  if(verbose) print(resp)

  ## handle errors and etc:
  parsed <- tw.httr(resp, simplifyDataFrame = TRUE)
  
  parsed <- data.table::as.data.table( parsed$phone_numbers )
  
  return(parsed)
}
