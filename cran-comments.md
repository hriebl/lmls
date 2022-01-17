## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Other remarks

* This is a resubmission. I would like to thank Gregor Seyer for his feedback on
  the first submission.
* The global environment is only modified in simulate() to restore the RNG state
  on exit if a seed is provided as an argument. This behavior is consistent with
  simulate() for an lm object. Gregor Seyer said he was okay with it in an email
  after the first submission.
