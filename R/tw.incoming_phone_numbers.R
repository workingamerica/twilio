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


# list phones -------------------------------------------------------------

#' Twilio Spoke API: List Owned Phones
#' 
#' @keywords twilio sms spoke api
#' @export tw.incoming_phone_numbers.list
#' 
#' @usage tw.incoming_phone_numbers.list(
#'   sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
#'   key = Sys.getenv('TWILIO_KEY'),
#'   secret = Sys.getenv('TWILIO_SECRET'),
#'   n = 20, 
#'   phone_number = NA, 
#'   friendly_name = NA, 
#'   verbose = FALSE
#' )
#' 
#' @param sid twilio credentials: Account SID.
#' @param key twilio credentials: Account user key.
#' @param secret twilio credentials: User secret. Don't store this in scripts!!!
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
#' tw.incoming_phone_numbers.list(sid, key, secret, n=20)
#' }



tw.incoming_phone_numbers.list <- function(
  sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
  key = Sys.getenv('TWILIO_KEY'),
  secret = Sys.getenv('TWILIO_SECRET'),
	n = 20, 
	phone_number = NA,
	friendly_name = NA, 
	verbose = FALSE
) {
  ## check for/create auth token:
  auth <- tw.auth(sid=key,token=secret)
  if(is.na(sid)) sid <- Sys.getenv('TWILIO_ACCOUNT_SID')
  
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
#' @usage tw.incoming_phone_numbers.fetch(
#'   sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
#'   key = Sys.getenv('TWILIO_KEY'),
#'   secret = Sys.getenv('TWILIO_SECRET'),
#'   phone_sid,
#'   verbose = FALSE
#'   )
#' 
#' @param sid twilio credentials: Account SID.
#' @param key twilio credentials: Account user key.
#' @param secret twilio credentials: User secret. Don't store this in scripts!!!
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

tw.incoming_phone_numbers.fetch <- function(
  sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
  key = Sys.getenv('TWILIO_KEY'),
  secret = Sys.getenv('TWILIO_SECRET'),
  phone_sid, 
  verbose = FALSE
  ) {
  ## check for/create auth token:
  auth <- tw.auth(sid=key,token=secret)
  if(is.na(sid)) sid <- Sys.getenv('TWILIO_ACCOUNT_SID')
  
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
#' @usage tw.incoming_phone_numbers.delete(
#'   sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
#'   key = Sys.getenv('TWILIO_KEY'),
#'   secret = Sys.getenv('TWILIO_SECRET'),
#'   phone_sid, 
#'   verbose = FALSE)
#' 
#' @param sid twilio credentials: Account SID.
#' @param key twilio credentials: Account user key.
#' @param secret twilio credentials: User secret. Don't store this in scripts!!!
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

tw.incoming_phone_numbers.delete <- function(
  sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
  key = Sys.getenv('TWILIO_KEY'),
  secret = Sys.getenv('TWILIO_SECRET'),
  phone_sid, 
  verbose = FALSE
  ) {
  ## check for/create auth token:
  auth <- tw.auth(sid=key,token=secret)
  if(is.na(sid)) sid <- Sys.getenv('TWILIO_ACCOUNT_SID')
  
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
#'   sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
#'   key = Sys.getenv('TWILIO_KEY'),
#'   secret = Sys.getenv('TWILIO_SECRET'),
#'   area_code = NA, 
#'   phone_number = NA,
#'   voice_url = '', 
#'   friendly_name = NA,
#'   verbose = FALSE )
#' 
#' @param sid twilio credentials: Account SID.
#' @param key twilio credentials: Account user key.
#' @param secret twilio credentials: User secret. Don't store this in scripts!!!
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
  sid = Sys.getenv('TWILIO_ACCOUNT_SID'),
  key = Sys.getenv('TWILIO_KEY'),
  secret = Sys.getenv('TWILIO_SECRET'),
  area_code = NA,
  phone_number = NA,
  voice_url = '',
  friendly_name = NA,
  verbose = FALSE
) {
  ## check for/create auth token:
  auth <- tw.auth(sid=key,token=secret)
  if(is.na(sid)) sid <- Sys.getenv('TWILIO_ACCOUNT_SID')
  
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
