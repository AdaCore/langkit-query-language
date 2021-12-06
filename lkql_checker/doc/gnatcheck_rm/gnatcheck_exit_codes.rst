.. _gnatcheck_Exit_Codes:

**********************
*gnatcheck* Exit Codes
**********************

.. index:: exit code

*gnatcheck* returns the following exit codes at the end of its run:

``0``
  No tool failure and no rule violation was detected.

``1``
  No fatal tool failure and at least one rule violation was detected.

``2``
  A fatal tool failure was detected, or a non-fatal tool failure was
  detected while no rule violation was detected (in this case the results
  of the gnatcheck run cannot de trusted).

``3``
  No Ada source file was checked.
