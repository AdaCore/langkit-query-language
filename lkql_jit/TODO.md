# TODOS

* Maybe don't use the LKQLContext to store the globals, do as Python do and store it in the first element of the frame
* Add the TruffleString support to avoid Java Strings
* Integrate the errors in the LKQL type system

# General ways of improvement

* Rework the JIT structure to make it more idiomatic
* Try to remove the more boundary as possible
* Enhance static analysis to optimize the execution