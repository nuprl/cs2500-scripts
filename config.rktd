((head-ta-name "Head TA")
 (head-ta-email "headta@ccs.neu.edu")
 (course-name "CS 2500")
 (server-dir "/srv/handin-server")
 (graded-dir "/srv/graded")
 (smtp-server "mail.example.com")
 (smtp-port 587)
 (smtp-user "user")
 (smtp-passwd "pass")
 (message-body 
   (lambda (grader-name grader-username grader-email problem-set-name) 
     (format 
"Hello ~a,

Attached are your assignments for problem set ~a."
grader-name problem-set-name))))
