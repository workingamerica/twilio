# .tw_get_phones: helper function to purchase twilio phones -----------------------------

#' Purchase Twilio Phones Algorithmically
#'
#' Intended use: Take a random sample of phones from the list you need to text.
#' Use this function to actually purchase and add to an existing message service.
#'
#' @name .tw_get_phones
#' @keywords twilio message service spoke
#' @export 
#'
#' @usage .tw_get_phones(
#'   phone,
#'   locality = NA,
#'   state = NA, 
#'   project, 
#'   max_dist, 
#'   msgsvc_sid, 
#'   sid, token, 
#'   progress=FALSE, verbose=FALSE)
#'
#' @param phone Single phone number to match, 10 digit (2025555555) or 12 digit (+12025555555), or 3-digit area code. TODO: Other formats.
#' @param locality Optional vector of localities, as a supplement or alternative to providing phones/area codes.
#' @param state Optional vector of states --- if \code{state} is supplied & twilio's response phone is out of state, phone will not be purchased.
#' @param project Project name---either "Working America" or "Mercury Opinion". Will be incorporated into phone Friendly Name.
#' @param msgsvc_sid The message service SID to add the phone to.
#' @param sid Twilio account SID
#' @param token Twilio account Token
#' @param progress Show progress bar --- disabled by default when this is used outside context of \code{tw_get_phones()}.
#' @param max_dist Maximum search distance from provided phone
#' @param verbose For error diagnosis.
#'
#' @examples
#'
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#' 
#' # NOTE: you could use the internal function that takes a single phone at a time 
#' # if you wanted to, for some reason.
#' .tw_get_phones <- function(phone, state, project, msgsvc_sid, sid, token)
#'
#' }

.tw_find_phones <- function(
  phone,
  locality=NA,
  state=NA,
  max_dist,
  sid,
  token,
  progress=FALSE,
  verbose=FALSE
) {
  
  if(nchar(phone)>3) { ## full phone is supplied
    
    ### TEST FOR GEOCODE AVAILABILITY:
    p <- try(
      tw.available_phone_local(sid,token, near_number = phone, distance = 1)
    )
    # if no phones, this returns list(), which has length==0.
    
    # deal w/ try error:
    if( inherits(p, "try-error") ){
      if(verbose) message('tw.available_phone_local returned an error when looking up ', phone)
      # give up and return empty
      p <- data.frame(matched_cell = phone, friendly_name = 'miscellaneous twilio error')
      return(p)
    }
    
    # if no matching phones found, DISTANCE SEARCH:
    # note: geocode error will have length 4, so it won't go into this part!
    # only gets here if it's got a legit lat/lon but no matching phones
    else if( length(p)==0 ){
      
      # search within (10, 60, 110, ..., max_dist) miles of the original phone.
      for( d in seq(from=10, to=max_dist+1, by=50) ) {
        if(verbose) {message(d,' miles: '); print(p)} 
        p <- try(
          tw.available_phone_local(sid,token, near_number = phone, distance = d)
        )
        # if no phones, this returns list(), which has length==0. if phones returned, return early!
        if(length(p)!=0 & is.null(p$code)) {
          # save the distance in here:
          p$search_distance <- d
          return(p)
        }
      }
    }
  }
  
  ## either they supplied an area code, or we'll cut it down to one
  ## (can help if no cell geocoding is the issue)
  
  # get area code:
  if(nchar(phone)==10) areacode <- substr(phone,1,3)
  else if(nchar(phone)==12 & substr(phone,1,2)=="+1") areacode <- substr(phone,3,5)
  else if(nchar(phone)==11 & substr(phone,1,1)=="1") areacode <- substr(phone,2,4)
  else stop("weird phone formatting, add an area code substring option: ",phone)
  
  if(verbose) {message('by area code: '); print(p)} 
  p <- tw.available_phone_local(sid,token, area_code = areacode)
  # success?
  if(length(p)!=0 & is.null(p$code)) return(p)
  
  # still none? try locality
  if(!is.na(locality)) {
    if(verbose) {message('by locality: '); print(locality)} 
    p <- tw.available_phone_local(sid,token, in_locality = locality)
    # success?
    if(length(p)!=0 & is.null(p$code)) return(p)
  }
  
  # *still* no phones?
  if(length(p)==0) {
    # give up and return empty
    p <- data.frame(matched_cell = phone, friendly_name = 'no nearby phones')
    return(p)
  }
  
}



.tw_get_phones <- function(
  phone,
  locality=NA,
  state=NA,
  project,
  max_dist,
  msgsvc_sid,
  sid,
  token,
  progress=FALSE,
  verbose=FALSE
) {
  # use the .tw_find_phones function:
  p <- .tw_find_phones(
    phone,
    locality,
    state,
    max_dist,
    sid,
    token,
    verbose
  )
  
  # if some kind of finding error is returned:
  if(length(p)!=13) {
    return(p)
  }
  
  p$capabilities <- NULL # this messes up the returned data frame.
  
  # wrong state
  if(!(p$region %in% state) & !is.na(state)) {
    if(verbose) message('wrong state!')
    p <- data.frame(
      matched_cell = phone,
      region = p$region,
      friendly_name = 'wrong state')
    return(p)
  }
  
  # if it worked right, save which phone you were searching on:
  p$matched_cell <- phone
  
  # purchase! :O
  purch <- try(tw.incoming_phone_numbers.create(
    sid,token,
    phone_number = p$phone_number,
    friendly_name = paste0(project," (",p$region,")")
  ))
  if(verbose) {message("Result of purchase attempt:"); print(purch)}
  if( inherits(purch, "try-error")) {
    message("\nTwilio error while matching phone ",phone,". Couldn't purchase ",p$phone_number)
    if(nchar(purch$message)>0) message(purch$message)
    p <- data.frame(matched_cell = phone, friendly_name = 'twilio purchase error')
    return(p)
  } #else if( purch$code==400 ) {
  #   message("\nTwilio error while matching phone ",phone,". Couldn't purchase ",p$phone_number)
  #   if(nchar(purch$message)>0) message(purch$message)
  #   p <- data.frame(matched_cell = phone, friendly_name = 'twilio purchase error')
  #   return(p)
  # }
  
  p$sid <- purch$sid
  p$friendly_name <- purch$friendly_name
  
  # add to msg svc:
  add_result <- try(
    tw.messaging_services.phone.add(sid, token, service_sid = msgsvc_sid, phone_sid = purch$sid)
  )
  if( inherits(add_result, "try-error") ) {
    message("\nTwilio error after purchasing. Couldn't add ", purch$sid," to ",msgsvc_sid)
    p <- data.frame(matched_cell = phone, friendly_name = 'twilio add error')
    return(p)
  }
  # if something goes wrong, there'll be a status code:
  if(!is.null(add_result$status)) {
    print(add_result)
    message("\nCould not add ",purch$sid," to ",msgsvc_sid)
    return(p)
  }
  p$msgsvc_sid <- msgsvc_sid
  if(!progress | verbose) message("\nSuccess: ",p$phone_number," in ",p$region," added to ",msgsvc_sid)
  
  p$capabilities <- NULL # this messes up the returned data frame.
  
  return(p)
}

# tw_get_phones: purchase twilio phones -----------------------------

#' Purchase Twilio Phones Algorithmically
#'
#' Intended use: Take a random sample of phones from the list you need to text.
#' Use this function to actually purchase and add to an existing message service.
#'
#' @keywords twilio message service spoke
#' @export tw_get_phones
#'
#' @usage tw_get_phones(
#'   phones, 
#'   locality = NA,
#'   state = NA, 
#'   project, 
#'   msgsvc_sid, 
#'   sid, token, 
#'   max_dist=75
#'   )
#'
#' @param phones Vector of phones number to match, 10 digit (2025555555) or 12 digit (+12025555555), or 3-digit area code. TODO: Other formats.
#' @param locality Optional vector of localities, as a supplement or alternative to providing phones/area codes.
#' @param state Optional vector of states --- if \code{state} is supplied & twilio's response phone is out of state, phone will not be purchased.
#' @param project Project name---either "Working America" or "Mercury Opinion". Will be incorporated into phone Friendly Name.
#' @param msgsvc_sid The message service SID to add the phone to.
#' @param sid Twilio account SID
#' @param token Twilio account Token
#' @param max_dist Maximum search distance from provided phone. Default 75 miles.
#'
#' @examples
#'
#' \dontrun{
#' source("C:/Users/lwolberg/Desktop/.ssh/twilio_auth.R")
#'
#' x <- fr(filename)
#'
#' # number of phones to buy
#' (nph <- ceiling(nrow(x)/200))
#'
#' # before sample, remove low-traffic area codes
#' if(nchar(x$cell[1])==10) {
#' x[, areacode := substr(cell,1,3)]
#' } else if (nchar(x$cell[1])==12) {
#'   x[, areacode := substr(cell,3,5)]
#' } else stop("weird phone formatting, add an area code substring option.")
#' x
#' x %>%
#'   group_by(areacode) %>%
#'   tally %>%
#'   arrange(-n) %>%
#'   print(n=100)
#' (limit <- nrow(x)*.001)
#' # limit = 30 # if the limit is too low to make sense.
#' x %>%
#'   group_by(areacode) %>%
#'   tally %>%
#'   filter(n<limit) %>%
#'   as.data.table -> ac_rm
#' x[, sample := 1]
#' x[areacode %in% ac_rm$areacode, sample := 0]
#'
#' phsamp <- sample(x[sample==1, cell], size=nph, replace=F)
#'
#' y <- tw_get_phones(
#'   phones = phsamp,
#'   state = NA,
#'   project = "Mercury Opinion",
#'   msgsvc_sid = "MGaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
#'   sid=sid, token=token
#' )
#'
#' }


tw_get_phones <- function(phones, locality=NA, state=NA, project, msgsvc_sid, sid, token, max_dist=75) {
  # check for message service SID:
  if(is.na(msgsvc_sid)) stop("Please supply a message service SID.")
  if(nchar(msgsvc_sid)!=34 | substr(msgsvc_sid,1,2)!="MG") stop("Message service SID is invalid.")
  existing <- tw.messaging_services.phone.list(sid, token, service_sid=msgsvc_sid)
  message("There are ", nrow(existing), " existing phones in this message service.")
  
  result <- plyr::ldply(
    .data = phones,
    .fun = .tw_get_phones,
    # .parallel = TRUE,
    state = state,
    locality = locality,
    project = project,
    msgsvc_sid = msgsvc_sid,
    sid=sid, token=token, max_dist = max_dist,
    .progress="text", progress=TRUE
  )
  
  existing <- tw.messaging_services.phone.list(sid, token, service_sid=msgsvc_sid)
  message("There are now ", nrow(existing), " phones in this message service.")
  
  result <- data.table::as.data.table(result) # i think this was the error?
  return(result)
}