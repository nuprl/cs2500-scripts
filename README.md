cs2500-scripts
==============

A package for scripting various tasks related to manaing students, grading, and the handin server. 

Install instructions
---

Ideally, this should be installable by:
```$ raco pkg install github://github.com/bluephoenix47/cs2500-scripts/master```

But I haven't tested that. Personally, I'm using it by:

```
$ git clone github://github.com/bluephoenix47/cs2500-scripts
$ cd cs2500-scripts
do stuff
```

Usage
---

This pacakge provides numerous interfaces to the handin server, assuming
a particular setup. Many things are customizable in `config.rktd`

###Managing student accounts

`cs2500-scripts/students.rkt` provides an interface to the handin users
(which I think of as students). This can be useful for automatically
manaing student accounts. `validate.rkt` uses this to ensure students
who create accounts are in the roster, automatically removing and
alerting students who create their account incorrectly. Honestly, it
might be easier on everyone to use the interface to create accounts from
the roster, because half of the student will create their account
incorrectly.

###Automating problem sets

`cs2500-scripts/problem-set.rkt` provides an interface to updating the
active and inactive directories of the handin server. `refresh.rkt`
demonstrates the problem set interface by updating the active and
inactive directories of the handin server. Note that it will *replace*
the active and inactive lists. crontab can be used to run `refresh.rkt`
automatically. An example crontab file exists in this package.

###Sending assignments to graders

`cs2500-scripts/grading.rkt` provides a bunch of losely related functions
for dealing with grades and grading. To send assignments to graders, use
`send-assignments-to-graders`. See `refresh.rkt` for an example, and
`crontab` for automating,

###Posting grades

`cs2500-scripts/grading.rkt` provides a bunch of losely related functions
for dealing with grades and grading. To post grades for quizzes or
exams, use grades->handin. update-grades.rkt is a convenient script for
command line use. 

It expects the grades to exist in a file `grades.rkt`. This file
should be a `read`able list of pairs of student usernames to grades,
as rational integers. For instance:
```
  ((student1 1/1)
   (student2 0/1))
```

Note the lack of `quote`, half points, and the seemingly redundant
use of a rational to represent 0 and 1.

For regular assignments using Eli's annotation language, and assuming
graders return zip files in the right format, you can post assignment
grades via `unzip graded-dir/hw1/grader/hw1.zip -d
handin-server/hw/hw1`. Or whatever.

Managing grades/Eli's scripts
---

Eli's crazy grading scripts are included in this pacakge under
eli-scripts. They provide all kinds of neat things, like automatically
create `grade` files based on a fancy annotation language, managing a
gradebook, creating per student summaries, and stuff. Copy the contents
to a convienent place, like `~/bin`, go through the README, and don't
ask questions. I've stripped all but the most vital (IMHO) parts of
Eli's script, so ignore anything the README references that doesn't
exist.
