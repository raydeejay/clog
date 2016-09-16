# CLOG, a static blog generator
A simple static blog generator written in Common Lisp.

### How to use
I haven't automated these steps yet:

1. Copy the skeleton directory to a place of your choosing
2. Copy the example.clogrc file to *~/.clogrc*, and edit it
3. Load Clog with (assuming you put it into your local-projects
   directory or equivalent): `(ql:quickload :clog)`
4. Evaluate `(clog:export-blog)`


Note that the workflow is very likely to change in the next
iteration(s). This is just an intermediate step.
