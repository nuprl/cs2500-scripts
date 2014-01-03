cs2500-scripts
==============

A package for scripting various tasks related to manaing students, grading, and the handin server. 

Install instructions
===

```raco pkg install github://github.com/bluephoenix47/cs2500-scripts/master```

Usage
===
This pacakge provides numerous interfaces to the handin server, assuming
a particular setup. Many things are customizable in config.rktd

Automating problem sets
----
refresh.rkt uses the problem set interface to the handin server to
enable automatically updating the active and inactive directories of the
handin server. Note that it will *replace* the active and inactive
lists. crontab can be used to run refresh automatically. An example
crontab file exists in this package.

Updating grades
----
Assuming grades
