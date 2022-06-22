#' Create a new rule repository
#' 
#' @param path \code{[character]} directory for the new repository. The directory
#' is created if it does not exist.
#' 
#' @returns A connection to rule repository
#' @export 
new_sqlite_repo <- function(path="./"){
  if (!dir.exists(path)) dir.create(path,recursive = TRUE)
  repofile <- file.path(path, "repository.sqlite")
  DBI::dbConnect(RSQLite::SQLite(), dbname=repofile)
}

#' @rdname new_sqlite_repo
open_sqlite_repo <- function(path="./"){
  dbname <- file.path(path,"repository.sqlite")
  x <- DBI::dbConnect(RSQLite::SQLite(), dbname=dbname)
  repos <- dir(path=path, pattern="\\.sqlite$",full.names = TRUE)
  repos <- setdiff(repos, dbname)
  for (repo in repos){
    repo_name <- sub("\\..*","",basename(repo))
    sql <- sprintf("attach database [%s] as %s", repo,  repo_name)
    DBI::dbExecute(x,sql)
    cat(sprintf("\n Attaching manager: %s",basename(repo_name)))
  }
  cat("\n")
  x
}

#' @param connection the SQLite db connection to close
#' @rdname new_sqlite_repo
#' @export
close_sqlite_repo <- function(connection){
  DBI::dbClearResult()
  DBI::dbDisconnect(connection)
}


#' Create a new rule manager
#'
#' @param repo A DBI connection to a rule repository
#' @param name Name of the rule manager
#'
#' @return Error
#'
#' @export
new_rulemanager <- function(repo, name, ...){
  UseMethod("new_rulemanager")
}


check_name <- function(x){
  if ( !grepl("^[a-zA-Z0-9_]+$",x)){
    msg <- sprintf("Invalid characters detected in '%s'.",x) 
    alw <- "Allowed characters are: a-z A-Z 0-9 and _"
    stop(paste(msg, alw,sep=" "), call.=FALSE)
  }
}

#' @rdname new_rulemanager
#' @export
new_rulemanager.SQLiteConnection <- function(connection, name,...){
  check_name(name)
  rules <- sprintf("%s_rules",name)
  rulesets <- sprintf("%s_rulesets",name)
  
  sql <- character()
  path <- file.path(dirname(DBI::dbGetInfo(con)$dbname), name)
    
  sql[1] <- " 
  attach database [%s.sqlite] as %s;
  " |> sprintf(path, name)

# repository of rules
  sql[2] <- "
  create table %s.rules(
  id              integer primary key,
  name            text not null,
  rule            text not null,
  label           text,
  description     text,
  valid_from      text,
  valid_to        text,
  added_by        text,
  removed_by      text,
  added_comment   text,
  removed_comment text
  );
  " |> sprintf(name)
 
# table registring rule lists and their meta data.
  sql[3] <- "
  create table %s.rule_lists(
  id               integer primary key,
  name             text not null unique,
  description      text,
  exists_from      text, 
  exists_to        text,
  added_by         text,
  removed_by       text,
  adding_comment   text,
  removing_comment text
  );
  " |> sprintf(name)
  
 # table registring set membership
  sql[4] <- "
  create table %s.membership(
  id              integer primary key,
  rule            text,    /* foreign key: rule id */
  list            text,    /* foreign key: ruleset id */
  index           text,    /* position of rule in ruleset */
  member_from     text,    /* start of membership */
  member_to       text,    /* end of membership or NULL */
  user_added      text,    /* user who added the rule to the ruleset */
  user_removed    text,    /* user who removed the rule to the ruleset */
  added_comment   text,    /* 
  removed_comment text
  );
  " |> sprintf(name)
  
  for (statement in sql) DBI::dbExecute(con, statement)
  NULL
}

#' Get pointer to rulemanager in repository
#' 
#' @param repo Open DBI connection to a rule repository.
#' @param manager name of rulemanager
#' 
#' @return If \code{manager} exists in \code{repo}, a DBI connection with
#' \code{repository} as attrite. Errors if \code{manager} does not exist.
#' 
#' @export
get_rulemanager <- function(repo, manager){
  UseMethod("get_repo")
}

#' @rdname get_rulemanager
get_rulemanager.RSQLiteConnection <- function(con, manager){
  managers <- dbGetQuery(con, "pragma database_list;")
  if (!name %in% managers$name){
    msg <- sprintf("'%s' was not found in this repository.", name)
    hnt <- "Use list_managers() for an overview of attached rule managers"
    stop(paste(msg,hnt), call. = FALSE)
  }
  attr(con,"manager") <- manager
  con
}


#' list available rule managers
#' 
#' @param repo Open DBI connection to a rule repository
#' @return \code{caracter} vector with list of attached rule managers.
#' @export 
list_managers <- function(repo){
  UseMethod("list_rulemanagers")
}

#' @rdname list_managers
#' @export
list_managers <- function(repo){
  d <- dbGetQuery(repo,"pragma database_list")
  d$name[!d$name %in% c("main","test") ]
}


#' Create new ruleset
#' 
#' @param manager A DBI connection labeled with a manager (use \code{\link{get_manager}})
#'        to create one.
#' @param name \code{[character]} name of new ruleset
#' @param valid \code{[POSIXct]} Either \code{NULL}, in which case the ruleset is valid
#' from the time when the function is called.  Or one or two \code{POSIXct} time stamps,
#' giving the begin and possibly the end time of the validity of the ruleset.
#' @param description \code{[character]} Optionally, a description of the rule set.
#' @param comment \code{[character]} Optionally, a description of the current submission.
#' 
#' @export
new_ruleset <- function(manager, ...){
  UseMethod("new_ruleset")
}

#' @rdname new_ruleset
#' @export
new_ruleset.RSQLiteConnection <- function(manager, name, valid=NULL, description=NULL){
  
}

upload_rules <- function(con, validator, add_to_set=NULL){
  
}


