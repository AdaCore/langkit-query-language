# TODOS

## LKQL JIT important tasks

* Make LKQL values use the interop library and DynamicObject
* Add wrapping classes for Node, Token and AnalysisUnit to make them usable by interop lib
* Make LKQL type meta-objects as DynamicObject and create Method class as a subclass of Function
* Perform unification on different list types (All list types should have the same type)
* Move features from the language to the launchers
* Add the TruffleString support to avoid Java Strings
* Unify all built-ins because selectors and functions are quite the same
* Unify node names
* Rework exceptions to separate static, dynamic and launchers exception