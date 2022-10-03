#' @import RSQLite
#' @import validate
#' @import methods
NULL


# Utilities ----

msgf  <- function(fmt,...) message(sprintf(fmt,...))
stopf <- function(fmt,...) stop(sprintf(fmt,...))

now <- function() as.character(Sys.time())

# monotone increasing sequence
mseq <- function(from, to) if (from > to) integer(0) else seq(from, to)


to_title_case <- function(x){
  substr(x,1,1) <- toupper(substr(x,1,1))
  x
}

#' Create, open, close repo ----


#' Create a new rule repository
#'
#' @param path \code{[character]} valid file location for sqlite database.
#' @param ... Currently unused
#'
#' @return a database connection to the rule repository
#'
#' @examples
#'
#' \dontrun{
#'  new_repo("my_rulez.sqlite")
#' }
#' @export
new_repo <- function(path,...){
  if (file.exists(path)) 
    stop("Trying to create a new repo in existing database. Quitting.")


  # create repo, report side effects.
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=path)
  msgf("Created %s",path)
  
  # Create tables 
  sql_rule <- "
    create table Rule(
      Id          integer primary key,
      Expression  text not null,
      Name        text not null,
      Description text
    );
  "  
  
  sql_ruleseq <- "
    create table RuleSequence(
      Id          integer primary key,
      Name        text not null,
      Description text
    );
  "

  sql_inseq <- "
    create table InSequence(
      Id     integer primary key,
      RuleId integer,
      SeqId  integer,
      Position     integer,
      Start        text,
      End          text,
      StartComment text,
      EndComment   text,
      foreign key (RuleId)
        references Rule (Id)
          on delete no action
          on update no action,
      foreign key (SeqId)
        references RuleSequence (Id)
          on delete no action
          on update no action
    );
  "

  RSQLite::dbClearResult(RSQLite::dbSendStatement(db, sql_rule))
  RSQLite::dbClearResult(RSQLite::dbSendStatement(db, sql_ruleseq))
  RSQLite::dbClearResult(RSQLite::dbSendStatement(db, sql_inseq))
  
  msgf("Created new repository.")

  db
}


#' Open an existing repository
#'
#' Attempts to open the sqlite database, and if it exists verifies that the
#' necessary tables and fields exist.
#'
#' @param path \code{[character]} location of existing sqlite rule repository.
#' @param ... Currently unused.
#'
#' @return a database connection to the rule repository.
#'
#' @export
open_repo <- function(path,...){
  if (!file.exists(path)) stopf("File location\n %s\ncould not be found",path)
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = path)
 
  expected <- list(
     Rule = c("Id", "Expression", "Name", "Description")
   , RuleSequence = c("Id","Name", "Description")
   , InSequence   = c("Id","RuleId", "SeqId","Position","Start","End", "StartComment", "EndComment")
  )
  expected_tables <- names(expected)
  tables <- RSQLite::dbListTables(db)
  if (!all(expected_tables %in% tables))
    stopf("%s is an SQLite database, but not a rule repository")


  for (tab in expected_tables){
    fields <- dbListFields(db, tab)
    if (!all(expected[[tab]] %in% fields))
     stopf("%s is not a valid rule repository")
  }

  db

}

#' Close a repository
#'
#' @param repo \code{[SQLiteConnection]}
#'
#' @export
close_repo <- function(repo) RSQLite::dbDisconnect(repo)


#' Create a new rule sequence
#'
#' A rule sequence is a list of rules intended for a certain purpose.
#' It may be altered, updated or reordered.
#'
#' @param repo \code{[SQLiteConnection]} as created by 
#'        \code{\link{new_repo}} or \code{\link{open_repo}}
#' @param name \code{[character]} name of the new sequence.
#' @param description \code{[character]} (optional) description.
#'
#'
#' @return The \code{repo}.
#' @export
add_rule_sequence <- function(repo, name, description=NA_character_){
  d <- data.frame( Name=as.character(name)
                 , Description=as.character(description))
  RSQLite::dbWriteTable(repo, "RuleSequence", d,append=TRUE)
  invisible(repo)

}


#' CRUD ----

#' Add rules to a repository
#'
#' @param repo \code{[SQLiteConnection]} as created by 
#'        \code{\link{new_repo}} or \code{\link{open_repo}}
#' @param rules A rule set
#' @param ... passed to other methods
#'
#' @return The \code{repo}
#' @export
setGeneric("add_rules",
  def = function(repo, rules,...) standardGeneric("add_rules")
)

#' @rdname add_rules
#' @export
setMethod("add_rules", signature = c("SQLiteConnection","data.frame")
  , definition = function(repo, rules){

      colnames <- c("Expression","Name", "Description")
      if (!identical(names(rules), colnames)){
        stopf("Missing %s from rule definition"
            , paste(colnames[!colnames %in% names(rules)], sep=", "))
      }
      ._add_rules(repo, rules)
      invisible(repo)
   }

)

#' @rdname add_rules
#' @export
setMethod("add_rules", signature = c("SQLiteConnection","validator")
  , definition = function(repo, rules){
      d <- as.data.frame(rules, lin_eq_eps=FALSE
                              , lin_ineq_eps=FALSE
                              , vectorize=FALSE)
      d <- d[c("name","rule","description")]
      names(d) <- c("Name","Expression","Description") 
      ._add_rules(repo, d)
      invisible(repo)
  }
)


#' Get all rules from a repo
#'
#' @param repo \code{[SQLiteConnection]} as created by 
#'        \code{\link{new_repo}} or \code{\link{open_repo}}
#'
#' @export
get_rules <- function(repo){
 suppressWarnings(RSQLite::dbReadTable(repo, "Rule"))
}


# worker function adding a rules to the Rule table.
._add_rules <- function(repo, rules){
  suppressWarnings(RSQLite::dbWriteTable(repo, "Rule", rules, append=TRUE))
}

# Get length of a rule sequence (current version)
seq_length <- function(repo, seq_id){
  sql <- sprintf("select max(Position) from InSequence 
                  where SeqId = %d and End IS NULL",seq_id)
  n = RSQLite::dbGetQuery(repo, sql)[1,1]
  if (is.na(n)) 0 else n
}

#' Remove a rule from a rule sequence
#'
#' @param repo \code{[SQLiteConnection]} as created by 
#'        \code{\link{new_repo}} or \code{\link{open_repo}}
#' @param rule_id \code{[integer]} Id of rule to remove from a sequence
#' @param seq_id \code{[integer]} Id of the sequence from which the rule is to be removed
#' @param position \code{[integer]} Position of the rule in te sequence
#' @param comment \code{[character]} removal comment.
#'
#' @export
remove_rule <- function(repo, seq_id, rule_id, position, comment=""){
  current_time <- now()
  n <- seq_length(repo, seq_id)

  sql <- sprintf("
          select RuleId, SeqId, Position from InSequence
          where End IS NULL and SeqId == %d and Position >= %d;
        ", seq_id, position)
    
  tab <- dbGetQuery(repo, sql)
  # remove, in current list (End IS NULL) all rules from 'position' up.
  for ( i in seq_len(nrow(tab)) ){
    sql <- sprintf(
           "update InSequence set End = '%s', EndComment = '%s'
            where End IS NULL and SeqId == %d and RuleId == %d and Position == %d;
           ", current_time, comment, tab[i,"SeqId"], tab[i,"RuleId"], tab[i,"Position"])
    RSQLite::dbClearResult(RSQLite::dbSendStatement(repo, sql))
  }

  # re-add the rules with position higher than the removed rule, each in one position lower.
  tab <- tab[-1,]
  for ( i in seq_len(nrow(tab)) ){
    sql <- sprintf(
           "insert into InSequence (RuleId, SeqId, Position, Start, StartComment)
            values (%d, %d, %d, '%s', '%s');
           ", tab[i,"RuleId"], tab[i,"SeqId"], tab[i,"Position"]-1, current_time, comment)
    RSQLite::dbClearResult(RSQLite::dbSendStatement(repo, sql))
  }

  repo
}


#' Insert a rule at a certain position
#'
#' @param repo \code{[SQLiteConnection]} as created by 
#'        \code{\link{new_repo}} or \code{\link{open_repo}}
#' @param rule_id \code{[integer]} Id of rule to remove from a sequence
#' @param seq_id \code{[integer]} Id of the sequence from which the rule is to be removed
#' @param position \code{[integer]} Position of the rule in te sequence
#' @param comment \code{[character]} removal comment.
#'
#' @return The repo, invisibly.
#' @export
insert_rule <- function(repo, rule_id, seq_id, position=NA_integer_, comment=""){
  current_time <- now()
  n <- seq_length(repo, seq_id)

  if (is.na(position)) position <- n + 1

  if (position > n+1){
    stopf("Trying to insert rule at position %d while current max position is %d", position, n)
  }
  
  cols <- "RuleId, SeqId, Position, Start, StartComment"

  vals <- sprintf("%d, %d, %d, '%s', '%s'"
                 , rule_id, seq_id, position, current_time, comment)
  if ( position == n+1 ){ 
    sql <- sprintf("insert into InSequence (%s) values (%s)", cols, vals)
    suppressWarnings(dbClearResult(dbSendQuery(repo, sql)))
    return(invisible(repo))
  }

  # Case where position != n+1
  
  # Remove all current (End IS NULL) sequence items above position
  sql <- sprintf("select RuleId, SeqId, Position, Start
                 from InSequence inner join Rule on InSequence.RuleId = Rule.Id 
                 where End IS NULL and SeqId == %d and Position >= %d
                 ", seq_id, position)
  tab <- dbGetQuery(repo, sql)
  for ( i in mseq(1, nrow(tab)) ){
    sql <- sprintf("update InSequence set End = '%s', EndComment = '%s'
                    where End IS NULL and SeqId == %d and RuleId == %d and Position == %d"
                    , current_time, comment, seq_id, tab[i,"RuleId"], tab[i,"Position"])
    rs <- RSQLite::dbSendStatement(repo, sql)
    RSQLite::dbClearResult(rs)
  }
  # Insert new rule into position
  sql <- sprintf("insert into InSequence (%s) values (%s)", cols, vals)
  suppressWarnings(dbClearResult(dbSendQuery(repo, sql)))

  # insert rules with updated positions
  tab$Position <- tab$Position + 1
  tab$Start    <- current_time
  tab$StartComment <- comment
  dbAppendTable(repo, "InSequence", tab)


  invisible(repo)

}

#' Get a rule sequence, as it was at a certain time.
#'
#' @param repo \code{[SQLiteConnection]} as created by 
#'        \code{\link{new_repo}} or \code{\link{open_repo}}
#' @param seq_id \code{[integer]} identity of the rule sequence
#' @param when \code{[character|POSIXct]} time stamp in "YYYY-MM-DD HH:MM:SS" format.
#'
#' @return A \code{data.frame} with Position, Name, Expression, sorted ascending
#'         by Position.
#' @export
get_rule_sequence <- function(repo, seq_id, when=NA_character_){
 

  if (is.na(when)){ 
    sql <- sprintf("select Position, Name, Expression
                    from InSequence inner join Rule on Rule.Id = InSequence.RuleId
                    where SeqId == %d and End IS NULL
                    order by Position ASC", seq_id)
    res <- RSQLite::dbSendQuery(repo,sql)
    out <- dbFetch(res, n=-1)
    dbClearResult(res)
    return(out)
  }

  timestamp <- strptime(as.character(when), "%Y-%m-%d %X")
  if (is.na(timestamp)){
    stopf("Timestamp must be in %%Y-%%m-%%d %%X format, e.g. '%s'",as.character(Sys.time()) )
  }


  sql <- sprintf("select POsition, Name, Expression
                  from InSequence inner join RUle on Rule.Id = InSequence.RuleId
                  where SeqId == %d and Start <= '%s' and (End IS NULL or End > '%s')
                  order by Position ASC",
                  seq_id, timestamp, timestamp)

  res <- RSQLite::dbSendQuery(repo,sql)
  out <- dbFetch(res, n=-1)
  dbClearResult(res)
  return(out)
#  return(RSQLite::dbFetch(RSQLite::dbSendQuery(repo,sql),n=-1))

}

#' Retrieve a list of all rule sequences
#'
#' List the rule sequences available in a rule repository.
#'
#' @param repo \code{[SQLiteConnection]} as created by 
#'        \code{\link{new_repo}} or \code{\link{open_repo}}
#'
#' @return A data frame
#' @export
get_sequences <- function(repo){
   dbReadTable(repo, "RuleSequence")
}


