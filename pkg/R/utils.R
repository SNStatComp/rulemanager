
# format time for insertion into database
format_time <- function(x){
  if (is.null(x)) return(NA_character_)
  format(as.POSIXct(x), "%Y-%m-%d %H%m%S %Z")
}

# check if name abides by the rules.
check_name <- function(x){
  if ( !grepl("^[a-zA-Z0-9_]+$",x)){
    msg <- sprintf("Invalid characters detected in '%s'.",x) 
    alw <- "Allowed characters are: a-z A-Z 0-9 and _"
    stop(paste(msg, alw,sep=" "), call.=FALSE)
  }
}


