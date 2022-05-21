Standard Library
----------------

Builtin Functions
^^^^^^^^^^^^^^^^^

.. function:: base_name(str)

    Given a string that represents a file name, returns the basename

.. function:: children

    Yields all the descendants of the given node in the tree

.. function:: concat(lists)

    Given a list of lists, return a concatenated list

.. function:: doc(obj)

    Given any object, return the documentation associated with it

.. function:: get_builtin_methods_info()

    Return information about builtin methods

.. function:: get_symbols(package=())

    Given a module, return the symbols stored in it. If given no module, return the local symbols

.. function:: help(obj)

    Given any object, return formatted help for it

.. function:: img(val)

    Return a string representation of an object

.. function:: next_siblings

    Yields the siblings following the given node in the tree

.. function:: parent

    Yields the parents (ancestors) of the given node in the tree

.. function:: pattern(string_pattern, case_sensitive=true)

    Given a regex pattern string, create a pattern object

.. function:: prev_siblings

    Yields the siblings preceding the given node in the tree

.. function:: print(val, new_line=true)

    Built-in print function. Prints whatever is passed as an argument

.. function:: profile(obj)

    Given any object, if it is a callable, return its profile as text

.. function:: reduce(indexable, fn, init)

    Given a collection, a reduction function, and an initial value reduce the result

.. function:: super_types

    Given a TypeDecl node, yields all the super types of the type

.. function:: unique(indexable)



.. function:: units()

    Return an iterator on all units

Builtin Methods
^^^^^^^^^^^^^^^

.. method:: Str.base_name ({params})

    Given a string that represents a file name, returns the basename

.. method:: Node.children_count ({params})

    Given a node, return the count of its children

.. method:: List.concat ({params})

    Given a list of lists, return a concatenated list

.. method:: Str.contains ({params})

    Search for `to_find` in the given string. Return whether a match is found. ``to_find`` can be either a pattern or a string

.. method:: Object.doc ({params})

    Given any object, return the documentation associated with it

.. method:: Node.dump ({params})

    Given an ast node, return a structured dump of the subtree

.. method:: Token.end_column ({params})

    Return the column end

.. method:: Token.end_line ({params})

    Return the line end

.. method:: Str.ends_with ({params})

    Given a string, returns whether it ends with the given suffix

.. method:: Str.find ({params})

    Search for `to_find` in the given string. Return position of the match, or -1 if no match. ``to_find`` can be either a pattern or a string

.. method:: Namespace.get_symbols ({params})

    Given a module, return the symbols stored in it. If given no module, return the local symbols

.. method:: Object.help ({params})

    Given any object, return formatted help for it

.. method:: Object.img ({params})

    Return a string representation of an object

.. method:: Token.is_equivalent ({params})

    Return whether two tokens are structurally equivalent

.. method:: Str.is_lower_case ({params})

    Return whether the given string contains lower case characters only

.. method:: Str.is_mixed_case ({params})

    Return whether the given string is written in mixed case, that is, with only lower case characters except the first one and every character following an underscore

.. method:: Token.is_trivia ({params})

    Return whether this token is a trivia

.. method:: Str.is_upper_case ({params})

    Return whether the given string contains upper case characters only

.. method:: Node.kind ({params})

    Return the kind of this node, as a string

.. method:: Analysis_unit.name ({params})

    Return the name of this unit

.. method:: Token.next ({params})

    Return the next token

.. method:: Str.pattern ({params})

    Given a regex pattern string, create a pattern object

.. method:: Token.previous ({params})

    Return the previous token

.. method:: Object.print ({params})

    Built-in print function. Prints whatever is passed as an argument

.. method:: Object.profile ({params})

    Given any object, if it is a callable, return its profile as text

.. method:: Object.reduce ({params})

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: Analysis_unit.root ({params})

    Return the root for this unit

.. method:: Node.same_tokens ({params})

    Return whether two nodes have the same tokens, ignoring trivias

.. method:: Str.split ({params})

    Given a string, return an iterator on the words contained by str separated by separator

.. method:: Token.start_column ({params})

    Return the column start

.. method:: Token.start_line ({params})

    Return the line start

.. method:: Str.starts_with ({params})

    Given a string, returns whether it starts with the given prefix

.. method:: Str.substring ({params})

    Given a string and two indices (from and to), return the substring contained between indices from and to (both included)

.. method:: Node.text ({params})

    Given an ast node, return its text

.. method:: Object.to_list ({params})

    Transform an iterator into a list

.. method:: Str.to_lower_case ({params})

    Return the given string written with lower case characters only

.. method:: Node.tokens ({params})

    Given a node, return an iterator on its tokens

.. method:: Object.unique ({params})



.. method:: Token.unit ({params})

    Return the unit for this token
