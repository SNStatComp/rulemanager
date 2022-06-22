# rulemanager

Management system for rule-driven statistical production systems. 

Status: **EXPERIMENTAL**


In a rule-driven production system, statisticians do not touch the data. In
stead, data processing is fully automated, where all data transformations are
driven by externally defined rules. Generally we distinguish between two types
of rules:

- [Data validation](https://arxiv.org/abs/2012.12028) rules. These are rules that
  express demands on a data set. A rule-driven system can ingest these rules and
  apply the transformations necessary to make a data set satisfy those rules.
- Data transformation rules. these are rules that describe how to alter existing values,
  fill in missing data, derive new variables, or derive new data structures (e.g.
  aggregation, modeling).

The system in this repository offers a minimal subset of rule management
features that focus on CRUD and reproducibility of production. The idea is to
offer a data structure and an extensible API that offer fundamental operations
on rules and rule sets that can be combined to build applications.

## User stories

The R package `rulemanager` aims to support the following user stories.

As a statistics producer, I want to 

1. Create, Update, and Delete rules so I can fix my current understanding
   of a statistical domain in the form of a formal ruleset.
2. Select a set of rules so I can apply them to my data.
3. Determine the order of rule execution so I have full control over
   data processing and validation.
4. Be able to trace the evolution of my rules and rule sets so I can (1) give full account
   of my production runs, and (2) reproduce production runs.
5. Temporarily remove a rule from one or more rule sets so I can handle exceptional and transient
   data circumstances. This temporary removal should be documented.
6. Temporarily update a rule from one or more rulesets so I can handle exceptional and transient
   data circumstances. This temoporary update should be documented.

## Design considerations and conditions


- Independence of database implementation. By default [SQLite](https://sqlite.org) is used
  but all fundamental operations are polymorphic and can be extended to other databases.


