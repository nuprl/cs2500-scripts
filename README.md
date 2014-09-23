cs2500-scripts
==============

A package for scripting various tasks related to managing students, grading, and the handin server.

Install instructions
---

To use this.

```
$ git clone github://github.com/bluephoenix47/cs2500-scripts
$ cd cs2500-scripts
do stuff
```

Usage
---

This package provides numerous interfaces to the handin server, assuming
a particular setup. Many things are customizable in `config.rktd`

###Managing/batch updating student accounts

`cs2500-scripts/students.rkt` provides an interface to the handin users
(which I think of as students). This can be useful for automatically
managing student accounts.

This interface assuming each user in the handin server has certain
fields in a particular order.

For instance, `validate.rkt` uses this to ensure students who create
accounts are actually in the roster by automatically removing accounts
not in `roster.rktd` or `whitelist.rktd`, and alerting students who
create their account incorrectly.

Another example, `update-students.rkt`, uses this interface to update
each students assigned grader.

###Automating due dates for problem sets

`cs2500-scripts/problem-set.rkt` provides an interface to updating the
active and inactive directories of the handin server.

For instance, `refresh.rkt` demonstrates the problem set interface by
updating the active and inactive directories of the handin server. Note
that it will *replace* the active and inactive lists. Cron can be
used to run scripts like `refresh.rkt` automatically. An example crontab
file exists in this package.

In practice, I assign due dates by creating a separate script for each
assignment, and entering each script with its due date in a crontab.

###Automatically sending assignments to graders

`cs2500-scripts/grading.rkt` provides a bunch of loosely related
functions for dealing with grades and grading. To send assignments to
graders, use `send-assignments-to-graders`.

For instance, `refresh.rkt` uses `send-assignments-to-graders` to send
all submitted assignments to the students' assigned graders, after
making the assignment inactive.

###Automatically posting grades

`cs2500-scripts/grading.rkt` provides a bunch of loosely related
functions for dealing with grades and grading.

For instance, `update-grades.rkt` is a convenient script for command
line use that will automatically unpack graded submissions from graders,
and post grades for exams and quizzes. I

The script `update-grades.rkt` assumes exam and quizzes are submitted as a
file named `grades.rkt`, an s-expr of lists of student user ids, as
symbols, and scores, as exact numbers.  This file should be a
`read`able. For instance:
```
  ((student1 1/1)
   (student2 0/1))
```

Note the lack of `quote`, half points, and the seemingly redundant
use of a rational to represent 0 and 1.

For regular assignments using Eli's annotation language, I assume
graders return zip files that can simply be unpacked to the handin
server. That is, I assumes you can post assignment grades via `unzip
graded-dir/hw1/grader/hw1.zip -d handin-server/hw/hw1`. The script
`update.sh` can be run via cron, and demonstrates how to automagically
update grades of all kinds every so often.

Eli's scripts
---
###Managing grades

Eli's crazy grading scripts are included in this package under
eli-scripts. They do all kinds of neat things, like automatically
compute scores and
create `grade` files based on an annotation language, manage a
gradebook, create per student summaries, and stuff. Copy the contents
to a convenient place, like `~/bin`, go through the README, and don't
ask questions. I've stripped all but the most vital (IMHO) parts of
Eli's script, so ignore anything the README references that doesn't
exist.

Essentially, `grades.rkt` needs to be copied to the handin server and modified with assignment data such as names (as used in the handin server), max points, and weight. Then, `compute-grades` needs to be updated to point to the handin server.

Then, simply run `computer-grades`. It will search the handin server for
grade files, update a grade book, and compute student summaries and
place them in the handin server directory.

### Dealing with honors
We now have an honors section that runs along side the regular section.
Dealing with both is troublesome since many students drop from honors to
regular halfway through, and thus we have to deal with a mix of
honors and regular assignments on a single server for the same student.

I've extended Eli's scripts a little. There are a few configuration variables specifically for honors. They are filled in with last fall's values as an example.

* honors-labs, which should be set to a list of strings such as '("lab7") which indicates which labs all the honors students attend. To use this feature, the scripts assume each student has a `lab` field in the handin server. The script `update-students.rkt` can be modified to update the `lab` field of each student in the handin server.

* regular-homeworks, which contains assignments (quizzes and exams included) to be graded only for non-honors students. Anything in this list is ignored for the honors students, i.e., treated as optional. It will show up in the handin server for them as not turned in, but won't be computed in their final grade.

* honors-homeworks which contains assignments to be graded only for honors students.  Anything in this list is ignored for the non-honors students, i.e., treated as optional. It will show up in the handin server for them as not turned in, but won't be computed in their final grade.

###WXME to text
The handin server accepts files in WXME format. This is not fun to
use. It can be configured to automagically textify things, but sometimes
you need to do that manually. `wxme-to-text` can help.
