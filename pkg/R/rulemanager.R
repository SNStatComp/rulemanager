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


#' @rdname new_rulemanager
#' @export
new_rulemanager.SQLiteConnection <- function(repo, name,...){
  check_name(name)
  
  sql <- character()
  path <- file.path(dirname(DBI::dbGetInfo(repo)$dbname), name)
    
  sql[1] <- " 
  attach database [%s.sqlite] as %s;
  " |> sprintf(path, name)
  RSQLite::dbExecute(repo, sql[1])

# repository of rules
  sql[2] <- "
  create table %s.rules(
  [id]              integer primary key autoincrement,
  [name]            text not null,
  [rule]            text not null,
  [label]           text,
  [description]     text,
  [valid_from]      text,
  [valid_to]        text,
  [added_by]        text,
  [removed_by]      text,
  [added_comment]   text,
  [removed_comment] text
  );
  " |> sprintf(name)
  RSQLite::dbExecute(repo, sql[2])
 
# table registring rule lists and their meta data.
  sql[3] <- "
  create table %s.rulesets(
 [id]              integer primary key autoincrement,
 [name]             text not null unique,
 [description]      text,
 [from]             text, 
 [until]            text,
 [added_by]         text,
 [removed_by]       text,
 [adding_comment]   text,
 [removing_comment] text
  );
  " |> sprintf(name)
  RSQLite::dbExecute(repo, sql[3])
  
 # table registring set membership
  sql[4] <- "
  create table %s.membership(
  [id]              integer primary key autoincrement,
  [rule]            integer,    /* foreign key: rule id */
  [list]            integer,    /* foreign key: rule list id */
  [index]           integer,    /* position of rule in ruleset */
  [member_from]     text,       /* start of membership */
  [member_to]       text,       /* end of membership or NULL */
  [user_added]      text,       /* user who added the rule to the ruleset */
  [user_removed]    text,       /* user who removed the rule to the ruleset */
  [added_comment]   text,       /* commit message for addition*/ 
  [removed_comment] text,       /* commit message for removal */
  UNIQUE(rule, list)            /* each rule may occur maximally once in a list */
  );
  " |> sprintf(name)
  RSQLite::dbExecute(repo, sql[4])
  
#  for (statement in sql[1:3]) DBI::dbExecute(repo, statement)
  invisible(repo)
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
  managers <- RSQLite::dbGetQuery(con, "pragma database_list;")
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
  UseMethod("list_managers")
}

#' @rdname list_managers
#' @export
list_managers.SQLiteConnection <- function(repo){
  d <- RSQLite::dbGetQuery(repo,"pragma database_list")
  d$name[!d$name %in% c("main","test") ]
}


#' Create new ruleset
#' 
#' @param repo An open DBI connection to a rule repository
#' @param manager \code{[character]} name of the rule manager
#' @param name \code{[character]} name of new ruleset to be created in the rule manager
#' @param description \code{[character]} A description of the rule set.
#' @param from Time from which the ruleset is valid.  Either \code{NA}, a
#'        \code{POSIXct} value or a string that can be converted to 
#'        \code{POSIXct} (see Details below).
#' @param user \code{[character]} User that added this ruleset.
#' @param comment \code{[character]} Commit message for creating the ruleset.
#' 
#' @section Details:
#' If \code{from} is \code{NA} it is assumed that the ruleset is valid since the
#' beginning of time.
#' 
#'
#' @export
new_ruleset <- function(repo, manager, name, ...){
  UseMethod("new_ruleset")
}



#' @rdname new_ruleset
#' @export
new_ruleset.SQLiteConnection <- function(repo, manager
                                          , name
                                          , description      = NA_character_
                                          , from             = Sys.time()
                                          , user             = Sys.info()['user']
                                          , comment          = NA_character_){

  df <- data.frame(
          name             = check_name(name)
        , description      = as.character(description)
        , from             = format_time(from)
        , until            = NA_character_
        , added_by         = user
        , removed_by       = NA_character_
        , adding_comment   = comment
        , removing_comment = NA_character_)

  RSQLite::dbAppendTable(repo, 
        DBI::dbQuoteIdentifier(DBI::ANSI()
                             , RSQLite::Id(schema=manager
                             , table='rulesets'))
      , df)
  invisible(repo)
}

upload_rules <- function(con, validator, add_to_set=NULL){
  
}


