# twilio auth check -------------------------------------------------------

#' Twilio Authentication Check
#' 
#' @keywords twilio sms spoke api
#' @export tw.auth
#' 
#' @usage tw.auth( sid, token )
#' 
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' 
#' @section Returns:
#' Object of class "request" (\code{httr::authenticate(sid,token)})
#' 
#' @section Permanently save twilio credentials as environment variables:
#' Run this:
#' \code{usethis::edit_r_environ()}
#' 
#' It'll open a file called \code{.Renviron}, which is saved in your home folder. (On Windows, this is of the form \code{C:/Users/myusername/Documents/.Renviron}.) Enter environment variables like so (no quotes), save, close, and restart R.
#' 
#' \code{TWILIO_ACCOUNT_SID = ACxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
#' \code{TWILIO_AUTH_TOKEN = xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
#' 
#' Now you can access environment variables using \code{Sys.getenv()}, and \code{tw.auth()} works by default.
#' 
#' @examples
#' 
#' tw.auth(sid="ACxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", token="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
#' 

tw.auth <- function(sid, token) {
  # default, usual values will be NA; then try env vars.
  if(is.na(sid)) sid <- Sys.getenv('TWILIO_ACCOUNT_SID')
  if(nchar(sid)!=34) stop("Please specify a valid SID, or add it as environment variable TWILIO_ACCOUNT_SID.",call.=F)
  
  if(is.na(token)) token <- Sys.getenv('TWILIO_AUTH_TOKEN')
  if(nchar(token)!=32) stop("Please specify a valid auth token, or add it as environment variable TWILIO_AUTH_TOKEN",call.=F)
  
  auth <- httr::authenticate(sid,token)
  return(auth)
}