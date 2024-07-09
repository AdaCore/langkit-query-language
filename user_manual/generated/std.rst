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

    Given an iterable object and a function, return the list resulting of the function application on each element of the iterable object: map(lst, f) -> [f(lst[1]), f(lst[2]), ...]

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

Builtin methods
^^^^^^^^^^^^^^^

Methods for `Any`
"""""""""""""""""
.. method:: Any.doc(this)

    Given any object, return the documentation associated with it

.. method:: Any.img(this)

    Return a string representation of an object

.. method:: Any.print(this, new_line)

    Built-in print function. Prints whatever is passed as an argument

Methods for `AnalysisUnit`
""""""""""""""""""""""""""
.. method:: AnalysisUnit.name(this)

    Return the name of this unit

.. method:: AnalysisUnit.root(this)

    Return the root for this unit

.. method:: AnalysisUnit.text(this)

    Return the text of the analysis unit

.. method:: AnalysisUnit.tokens(this)

    Return the tokens of the unit

Methods for `LazyList`
""""""""""""""""""""""
.. method:: LazyList.enumerate(this)

    Return the content of the iterable object with each element associated to its index in a tuple: [(<index>, <elem>), ...]

.. method:: LazyList.length(this)

    Get the length of the iterable element

.. method:: LazyList.map(this, fn)

    Given an iterable object and a function, return the list resulting of the function application on each element of the iterable object: map(lst, f) -> [f(lst[1]), f(lst[2]), ...]

.. method:: LazyList.reduce(this, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: LazyList.to_list(this)

    Transform an iterator into a list

Methods for `List`
""""""""""""""""""
.. method:: List.enumerate(this)

    Return the content of the iterable object with each element associated to its index in a tuple: [(<index>, <elem>), ...]

.. method:: List.length(this)

    Get the length of the iterable element

.. method:: List.map(this, fn)

    Given an iterable object and a function, return the list resulting of the function application on each element of the iterable object: map(lst, f) -> [f(lst[1]), f(lst[2]), ...]

.. method:: List.reduce(this, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: List.sublist(this, low_bound, high_bound)

    Return a sublist of `list` from `low_bound` to `high_bound`

.. method:: List.to_list(this)

    Transform an iterator into a list

.. method:: List.unique(this)

    Given collection, remove all identical elements in order to have only one instance of each

Methods for `Node`
""""""""""""""""""
.. method:: Node.children(this)

    Given a node, get the list of all its children

.. method:: Node.children_count(this)

    Given a node, return the count of its children

.. method:: Node.dump(this)

    Given an ast node, return a structured dump of the subtree

.. method:: Node.image(this)

    Given an ast node, return its image

.. method:: Node.kind(this)

    Return the kind of this node, as a string

.. method:: Node.parent(this)

    Given a node, get the parent of it

.. method:: Node.same_tokens(this, other)

    Return whether two nodes have the same tokens, ignoring trivias

.. method:: Node.text(this)

    Given an ast node, return its text

.. method:: Node.tokens(this)

    Given a node, return an iterator on its tokens

.. method:: Node.unit(this)

    Given an ast node, return its analysis unit

Methods for `SelectorList`
""""""""""""""""""""""""""
.. method:: SelectorList.enumerate(this)

    Return the content of the iterable object with each element associated to its index in a tuple: [(<index>, <elem>), ...]

.. method:: SelectorList.length(this)

    Get the length of the iterable element

.. method:: SelectorList.map(this, fn)

    Given an iterable object and a function, return the list resulting of the function application on each element of the iterable object: map(lst, f) -> [f(lst[1]), f(lst[2]), ...]

.. method:: SelectorList.reduce(this, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: SelectorList.to_list(this)

    Transform an iterator into a list

Methods for `Str`
"""""""""""""""""
.. method:: Str.base_name(this)

    Given a string that represents a file name, returns the basename

.. method:: Str.contains(this, to_find)

    Search for to_find in the given string. Return whether a match is found. to_find can be either a pattern or a string

.. method:: Str.ends_with(this, suffix)

    Given a string, returns whether it ends with the given suffix

.. method:: Str.find(this, to_find)

    Search for to_find in the given string. Return position of the match, or -1 if no match. to_find can be either a pattern or a string

.. method:: Str.is_lower_case(this)

    Return whether the given string contains lower case characters only

.. method:: Str.is_mixed_case(this)

    Return whether the given string is written in mixed case, that is, with only lower case characters except the first one and every character following an underscore

.. method:: Str.is_upper_case(this)

    Return whether the given string contains upper case characters only

.. method:: Str.length(this)

    Given a string, return the length of it in character

.. method:: Str.split(this, separator)

    Given a string, return an iterator on the words contained by str separated by separator

.. method:: Str.starts_with(this, prefix)

    Given a string, returns whether it starts with the given prefix

.. method:: Str.substring(this, from, to)

    Given a string and two indices (from and to), return the substring contained between indices from and to (both included)

.. method:: Str.to_lower_case(this)

    Return the given string written with lower case characters only

.. method:: Str.to_upper_case(this)

    Return the given string written with upper case characters only

Methods for `Token`
"""""""""""""""""""
.. method:: Token.end_column(this)

    Return the column end

.. method:: Token.end_line(this)

    Return the line end

.. method:: Token.is_equivalent(this, other)

    Return whether two tokens are structurally equivalent

.. method:: Token.is_trivia(this)

    Return whether this token is a trivia

.. method:: Token.kind(this)

    Return the kind of the token

.. method:: Token.next(this, exclude_trivia)

    Return the next token

.. method:: Token.previous(this, exclude_trivia)

    Return the previous token

.. method:: Token.start_column(this)

    Return the column start

.. method:: Token.start_line(this)

    Return the line start

.. method:: Token.text(this)

    Return the text of the token

.. method:: Token.unit(this)

    Return the unit for this token

