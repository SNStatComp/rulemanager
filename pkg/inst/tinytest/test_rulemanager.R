# placeholder for unit tests

con <- new_sqlite_repo(tempdir())
expect_true(file.exists(file.path(tempdir(),"repository.sqlite")))

new_rulemanager(con,"foo")
expect_equal(list_managers(con),"foo")

new_ruleset(con,"foo","bar")

DBI::dbDisconnect(con)

