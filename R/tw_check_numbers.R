#' Twilio Check Numbers API
#'
#' Get classification of phones from Twilio as mobile/landline/voip. Costs $0.005 per call, which adds up!
#' 
#' The intermediate results are written to files in your working directory ('twilio results [date] [time].csv'), to ensure minimal loss of data if the function is interrupted.
#' 
#' @name tw_check_numbers
#' @keywords twilio sms
#' @export tw_check_numbers
#' 
#' @usage tw_check_numbers(phones, sid = NA, token = NA, test = FALSE)
#' 
#' @param phones vector of 10-digit phone numbers, preferably as character strings.
#' @param sid twilio credentials: SID string. Don't store this in scripts!
#' @param token twilio credentials: Auth token string. Don't store this in scripts!
#' @param test test the progress bar
#' 
#' @examples
#' 
#' cost_limit = 1
#' p <- round(runif(10, min=2345678901, max=3456789012))
#' 
#' # source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' # Using the function:
#' # tw_check_numbers(p, sid = 'my_sid', token = 'my_token', test = TRUE)
#' 
#' # some good test data for error types:
#' # result <- tw_check_numbers( phones = c('7209339860','5555555555','2139416728'), sid, token) )


### TODO: budget as a token list? https://stackoverflow.com/questions/12598242/
### https://elbauldelprogramador.com/en/acceder-variable-dentro-funcion-r/

tw_check_numbers <- function(phones, sid = NA, token = NA, test = FALSE) {
  ## check for/create auth token:
  auth <- tw.auth(sid,token)
  
  ## because bit64 is a pain:
  phones <- as.character(phones)
  
  ## check number validity:
  if( length( phones[nchar(phones)!=10] )>0 ) {
    cont <- readline("Warning! Some phones not 10 digits long. Continue? y/n: ")
    if(tolower(cont) != "y") {
      stop("Execution stopped.", call.=F)
    }
  }
  
  ## is this within budget?
  cost <- length(phones)*0.005
  message("This API call will cost $",cost)
  cost_limit <- as.numeric(readline("Amount approved: $"))
  if(is.na(cost_limit)) cost_limit <- 0
  if(cost > cost_limit) {
    err_cost <- paste0("Cost not approved. $",cost," is over the approved limit of $",cost_limit)
    stop(err_cost, call.=F)
  }
  total_cost <- 0
  
  ## create progress bar
  pb <- progress::progress_bar$new(
    format = "  matching [:bar] :percent eta: :eta",
    total = length(phones), clear = FALSE, width= 70)
  
  ## initialize empty data.table:
  returns <- data.table::data.table(
    phone = character(),
    phone_type = character(),  # the thing we seek!
    phone_carrier = character(),  # paid for it, might as well keep it
    phone_lookupdate = as.POSIXct(character()), # https://stackoverflow.com/questions/24250463
    phone_tw_req_id = character(),
    phone_error = character()
  )
  
  for(p in phones) {
    ## update progress bar:
    pb$tick()
    
    ## generate API URL
    u <- paste0('https://lookups.twilio.com/v1/PhoneNumbers/',p,'?CountryCode=US&Type=carrier')
    
    ## get data (if it's not a test). This is the thing that costs the $!
    if(test) {
      Sys.sleep(1)
      next
    }
    else { resp <- httr::GET(url=u, auth ) }
    
    ## record the cost against global ledger:
    total_cost <- 0.005+total_cost # <<- assigns to a global not local var.
    
    ## handle errors:
    if(httr::http_type(resp) != "application/json"){
      warning("Twilio API did not return JSON for ",u,".")
      
      r <- data.table::data.table(
        phone = p,
        phone_type = NA,  # the thing we seek!
        phone_carrier = NA,  # paid for it, might as well keep it
        phone_lookupdate = NA, # https://stackoverflow.com/questions/24250463
        phone_tw_req_id = u,
        phone_error = "no JSON"
      )
    } else {
      ## parse the response:
      parsed <- jsonlite::fromJSON(
        httr::content(resp, "text", encoding = "UTF-8"), 
        simplifyVector = FALSE)
      
      ## handle errors: resp$status_code exists before parsing, so that's OK.
      #### 401: auth error
      #### not 200: other unsuccessful lookup
      #### 200: success!
      if(resp$status_code==401) {
        message("Error: Authentication error, check SID and Token.")
        return(list(returns,r))
      } else if(resp$status_code != 200) {
        r <- data.table::data.table(
          phone = p,
          phone_type = 'invalid#',
          phone_carrier = NA_character_,
          phone_lookupdate = resp$date,
          phone_tw_req_id = as.character(resp$headers$`twilio-request-id`),
          phone_error = paste(resp$status_code, parsed$carrier$error_code)
        )
        
      } else {
        r <- data.table::data.table(
          phone = parsed$phone_number,
          phone_type = parsed$carrier$type,  # the thing we seek!
          phone_carrier = parsed$carrier$name,  # paid for it, might as well keep it
          phone_lookupdate = resp$date, 
          phone_tw_req_id = as.character(resp$headers$`twilio-request-id`),
          phone_error = paste(resp$status_code, parsed$carrier$error_code)
        )
      }
    }
    
    
    ## save the response to return:
    returns <- rbind(returns,r, fill=T)
    
    ## randomly about 1/10 of the time, save results to a file, in case it gets interrupted!
    if(substr(u,50,50)==0) {
      # dir.create() does not crash if the directory already exists, it just prints out a warning.
      dir.create(file.path(getwd(), "twilio results"), showWarnings = FALSE)
      
      data.table::fwrite(
        returns, 
        paste("twilio results/twilio results",format(Sys.time(), "%Y-%m-%d %H.%M"),".csv")
        )
    }
    
  }
  message("Total Spent: $",total_cost)
  data.table::fwrite(
    returns, 
    paste("twilio results",format(Sys.time(), "%Y-%m-%d %H.%M")," - final.csv")
  )
  return(returns)
}
