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
  sid = NA, 
  token = NA,
  from = NA,
  to = NA,
  datesent = NA,
  n = 20,
  verbose = FALSE
) {
  ## check for/create auth token:
  auth <- tw.auth(sid,token)
  if(is.na(sid)) sid <- Sys.getenv('TWILIO_ACCOUNT_SID')
  
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