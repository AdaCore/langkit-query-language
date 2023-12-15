Standard library
----------------

Builtin functions
^^^^^^^^^^^^^^^^^

.. function:: print(val, new_line)

    Built-in print function. Prints whatever is passed as an argument

.. function:: img(val)

    Return a string representation of an object

.. function:: base_name(str)

    Given a string that represents a file name, returns the basename

.. function:: concat(lists)

    Given a list of lists or strings, return a concatenated list or string

.. function:: reduce(indexable, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. function:: map(indexable, fn)

    Given a collection, a mapping function

.. function:: unique(indexable)

    Given collection, remove all identical elements in order to have only one instance of each

.. function:: doc(obj)

    Given any object, return the documentation associated with it

.. function:: profile(obj)

    Given any object, if it is a callable, return its profile as text

.. function:: document_builtins()

    Return a string in the RsT format containing documentation for all built-ins

.. function:: document_namespace(namespace, name)

    Return a string in the RsT format containing documentation for all built-ins

.. function:: help(obj)

    Given any object, return formatted help for it

.. function:: units()

    Return an iterator on all units

.. function:: specified_units()

    Return an iterator on units specified by the user

.. function:: pattern(regex, case_sensitive)

    Given a regex pattern string, create a pattern object

.. function:: node_checker(root)

    Given a root, execute all node checker while traverse the tree

.. function:: unit_checker(unit)

    Given a unit, apply all the unit checker on it

Builtin selectors
^^^^^^^^^^^^^^^^^

.. function:: children()

    Yields all the descendants of the given node in the tree
    

.. function:: parent()

    Yields the parents (ancestors) of the given node in the tree
    

.. function:: next_siblings()

    Yields the siblings following the given node in the tree
    

.. function:: prev_siblings()

    Yields the siblings preceding the given node in the tree
    

.. function:: super_types()

    Given a TypeDecl node, yields all the super types of the type
    

Builtin methods
^^^^^^^^^^^^^^^

Methods for `Any`
"""""""""""""""""
.. method:: Any.doc(obj)

    Given any object, return the documentation associated with it

.. method:: Any.img(val)

    Return a string representation of an object

.. method:: Any.print(val, new_line)

    Built-in print function. Prints whatever is passed as an argument

Methods for `AnalysisUnit`
""""""""""""""""""""""""""
.. method:: AnalysisUnit.name(unit)

    Return the name of this unit

.. method:: AnalysisUnit.root(unit)

    Return the root for this unit

.. method:: AnalysisUnit.text(unit)

    Return the text of the analysis unit

.. method:: AnalysisUnit.tokens(unit)

    Return the tokens of the unit

Methods for `LazyList`
""""""""""""""""""""""
.. method:: LazyList.length(iterable)

    Get the length of the iterable element

.. method:: LazyList.reduce(indexable, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: LazyList.to_list(iterable)

    Transform an iterator into a list

Methods for `List`
""""""""""""""""""
.. method:: List.length(iterable)

    Get the length of the iterable element

.. method:: List.reduce(indexable, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: List.sublist(list, low_bound, high_bound)

    Return a sublist of `list` from `low_bound` to `high_bound`

.. method:: List.to_list(iterable)

    Transform an iterator into a list

.. method:: List.unique(indexable)

    Given collection, remove all identical elements in order to have only one instance of each

Methods for `Node`
""""""""""""""""""
.. method:: Node.children(node)

    Given a node, get the list of all its children

.. method:: Node.children_count(node)

    Given a node, return the count of its children

.. method:: Node.dump(node)

    Given an ast node, return a structured dump of the subtree

.. method:: Node.image(node)

    Given an ast node, return its image

.. method:: Node.kind(node)

    Return the kind of this node, as a string

.. method:: Node.parent(node)

    Given a node, get the parent of it

.. method:: Node.same_tokens(node, other)

    Return whether two nodes have the same tokens, ignoring trivias

.. method:: Node.text(node)

    Given an ast node, return its text

.. method:: Node.tokens(node)

    Given a node, return an iterator on its tokens

.. method:: Node.unit(node)

    Given an ast node, return its analysis unit

Methods for `SelectorList`
""""""""""""""""""""""""""
.. method:: SelectorList.length(iterable)

    Get the length of the iterable element

.. method:: SelectorList.reduce(indexable, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: SelectorList.to_list(iterable)

    Transform an iterator into a list

Methods for `Str`
"""""""""""""""""
.. method:: Str.base_name(str)

    Given a string that represents a file name, returns the basename

.. method:: Str.contains(str, to_find)

    Search for to_find in the given string. Return whether a match is found. to_find can be either a pattern or a string

.. method:: Str.ends_with(str, suffix)

    Given a string, returns whether it ends with the given suffix

.. method:: Str.find(str, to_find)

    Search for to_find in the given string. Return position of the match, or -1 if no match. to_find can be either a pattern or a string

.. method:: Str.is_lower_case(str)

    Return whether the given string contains lower case characters only

.. method:: Str.is_mixed_case(str)

    Return whether the given string is written in mixed case, that is, with only lower case characters except the first one and every character following an underscore

.. method:: Str.is_upper_case(str)

    Return whether the given string contains upper case characters only

.. method:: Str.length(str)

    Given a string, return the length of it in character

.. method:: Str.split(str, separator)

    Given a string, return an iterator on the words contained by str separated by separator

.. method:: Str.starts_with(str, prefix)

    Given a string, returns whether it starts with the given prefix

.. method:: Str.substring(str, from, to)

    Given a string and two indices (from and to), return the substring contained between indices from and to (both included)

.. method:: Str.to_lower_case(str)

    Return the given string written with lower case characters only

.. method:: Str.to_upper_case(str)

    Return the given string written with upper case characters only

Methods for `Token`
"""""""""""""""""""
.. method:: Token.end_column(token)

    Return the column end

.. method:: Token.end_line(token)

    Return the line end

.. method:: Token.is_equivalent(this, other)

    Return whether two tokens are structurally equivalent

.. method:: Token.is_trivia(token)

    Return whether this token is a trivia

.. method:: Token.kind(token)

    Return the kind of the token

.. method:: Token.next(token, exclude_trivia)

    Return the next token

.. method:: Token.previous(token, exclude_trivia)

    Return the previous token

.. method:: Token.start_column(token)

    Return the column start

.. method:: Token.start_line(token)

    Return the line start

.. method:: Token.text(token)

    Return the text of the token

.. method:: Token.unit(token)

    Return the unit for this token

