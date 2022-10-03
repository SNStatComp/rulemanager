# Set up a rulemanager, fill it with rules and assign them to 
# different rule sequences.


library(validate)
library(rulemanager)


# read example rules from file
rules <- validator(.file="rules.R")


# create a new repository 
if (file.exists("my_repo.sqlite")) unlink("my_repo.sqlite")

# this will set up a new database with the appropriate tables.
repo <- new_repo("my_repo.sqlite")

# upload rules to the repository
repo <- add_rules(repo, rules)

# Create some rule sequences
add_rule_sequence(repo
            , name = "SBS"
            , description = "Rules for the Structural Business Survey microdata")

add_rule_sequence(repo
            , name = "STS"
            , description = "Rules for the Short Term Statistics microdata")

for ( i in 1:10 ){
  insert_rule(repo, rule_id = i ,seq_id = 1L, position = i)
}

get_rule_sequence(repo,seq_id=1L)

# a peek under the hood (don't try this at home!)
RSQLite::dbReadTable(repo, "InSequence")

pos <- 1
for ( i in 5:18 ){
  insert_rule(repo, rule_id=i, seq_id=2L, position = pos)
  pos <- pos + 1
}


get_rule_sequence(repo, seq_id=2L)

# a peek under the hood (don't try this at home!)
RSQLite::dbReadTable(repo, "InSequence")

close_repo(repo)


