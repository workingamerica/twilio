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
  auth <- tw.auth(sid,token)
  
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

tw.messaging_services.list <- function(
  sid = NA, 
  token = NA, 
  n = 20, 
  alldata = FALSE, 
  verbose = FALSE) {
  ## check for/create auth token:
  auth <- tw.auth(sid,token)
  
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
  auth <- tw.auth(sid,token)
  
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
  auth <- tw.auth(sid,token)
  
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
  auth <- tw.auth(sid,token)
  
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
  auth <- tw.auth(sid,token)
  
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
  auth <- tw.auth(sid,token)
  
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