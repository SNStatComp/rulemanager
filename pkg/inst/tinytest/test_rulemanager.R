# placeholder for unit tests

con <- new_repository(tempdir())
new_rulemanager(con,"foo")
expect_true(file.exists(file.path(tempdir(),"foo"))

new_ruleset(con,"foo","bar")
expect_equal(list_managers(con),"bar")

