
library(rulemanager)

# open repository
repo <- open_repo("my_repo.sqlite")

# look at the available sequences
get_sequences(repo)

# get one of the rule sequences
get_rule_sequence(repo, 1)

# look at all the available rules
get_rules(repo)

# add a rule to the first sequence, in position 5
insert_rule(repo
            , rule_id=18
            , seq_id=1
            , position = 5
            , comment="new balance rule")

swap_rules(repo, seq_id=1, rule1=7, position1=7, rule2=9, position2=9)

get_rule_sequence(repo,1)

# Jump into your TARDIS and look at the old sequence
get_rule_sequence(repo, seq_id=1L, when = "2022-10-03 14:10:00")

remove_rule(repo,seq_id=1, rule_id=2, position=2, comment="get rid of it")

get_rule_sequence(repo, seq_id=1L)

# Or, get is straight as a validator object
rules <- get_validator(repo, seq_id=1L)
rules

# See the full historic record.
get_history(repo)

close_repo(repo)


