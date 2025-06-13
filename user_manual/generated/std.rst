Standard library
----------------

Builtin functions
^^^^^^^^^^^^^^^^^

.. function:: unique(indexable)

    Given a collection, create a list with all duplicates removed

.. function:: pattern(regex, case_sensitive)

    Given a regex pattern string, create a pattern object

.. function:: print(to_print, new_line)

    Built-in print function. Prints the argument

.. function:: img(string)

    Return a string representation of an object

.. function:: doc(value)

    Given any object, return the documentation associated with it

.. function:: reduce(iterable, function, init_value)

    Given a collection, a reduction function, and an initial value reduce the result

.. function:: document_builtins()

    Return a string in the RsT format containing documentation for all built-ins

.. function:: base_name(file_name)

    Given a string that represents a file name, returns the basename

.. function:: concat(list)

    Given a list, return the result of the concatenation of all its elements

.. function:: map(iterable, function)

    Given a collection, a mapping function

.. function:: profile(val)

    Given any object, if it is a callable, return its profile as text

.. function:: document_namespace(namespace, name)

    Return a string in the RsT format containing documentation for all built-ins

.. function:: help(value)

    Print formatted help for the given object

.. function:: units()

    Return a list of all units

.. function:: specified_units()

    Return a list of units specified by the user

.. function:: node_checker(root)

    Given a root, execute all node checkers while traversing the tree

.. function:: unit_checker(unit)

    Given a unit, apply all the unit checkers on it

Builtin methods
^^^^^^^^^^^^^^^

Methods for `AnalysisUnit`
""""""""""""""""""""""""""
.. method:: AnalysisUnit.doc(this)

    Given any object, return the documentation associated with it

.. method:: AnalysisUnit.help(this)

    Print formatted help for the given object

.. method:: AnalysisUnit.img(this)

    Return a string representation of an object

.. method:: AnalysisUnit.name(this)

    Return the name for this unit

.. method:: AnalysisUnit.print(this)

    Built-in print function. Prints the argument

.. method:: AnalysisUnit.root(this)

    Return the root for this unit

.. method:: AnalysisUnit.text(this)

    Return the text for this unit

.. method:: AnalysisUnit.tokens(this)

    Return the tokens for this unit

Methods for `Bool`
""""""""""""""""""
.. method:: Bool.doc(this)

    Given any object, return the documentation associated with it

.. method:: Bool.help(this)

    Print formatted help for the given object

.. method:: Bool.img(this)

    Return a string representation of an object

.. method:: Bool.print(this)

    Built-in print function. Prints the argument

Methods for `Function`
""""""""""""""""""""""
.. method:: Function.doc(this)

    Given any object, return the documentation associated with it

.. method:: Function.help(this)

    Print formatted help for the given object

.. method:: Function.img(this)

    Return a string representation of an object

.. method:: Function.print(this)

    Built-in print function. Prints the argument

Methods for `Int`
"""""""""""""""""
.. method:: Int.doc(this)

    Given any object, return the documentation associated with it

.. method:: Int.help(this)

    Print formatted help for the given object

.. method:: Int.img(this)

    Return a string representation of an object

.. method:: Int.print(this)

    Built-in print function. Prints the argument

Methods for `LazyList`
""""""""""""""""""""""
.. method:: LazyList.doc(this)

    Given any object, return the documentation associated with it

.. method:: LazyList.enumerate(this)

    Return the content of the iterable object with each element associated to its index in a tuple: [(<index>, <elem>), ...]

.. method:: LazyList.help(this)

    Print formatted help for the given object

.. method:: LazyList.img(this)

    Return a string representation of an object

.. method:: LazyList.length(this)

    Return the length of the iterable

.. method:: LazyList.print(this)

    Built-in print function. Prints the argument

.. method:: LazyList.reduce(this, function, init_value)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: LazyList.to_list(this)

    Transform into a list

Methods for `List`
""""""""""""""""""
.. method:: List.combine(this, right, recursive)

    Combine two LKQL values if possible and return the result, recursively if required

.. method:: List.doc(this)

    Given any object, return the documentation associated with it

.. method:: List.enumerate(this)

    Return the content of the iterable object with each element associated to its index in a tuple: [(<index>, <elem>), ...]

.. method:: List.help(this)

    Print formatted help for the given object

.. method:: List.img(this)

    Return a string representation of an object

.. method:: List.length(this)

    Return the length of the iterable

.. method:: List.print(this)

    Built-in print function. Prints the argument

.. method:: List.reduce(this, function, init_value)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: List.sublist(this, low, high)

    Return a sublist of `list` from `low_bound` to `high_bound`

.. method:: List.to_list(this)

    Transform into a list

.. method:: List.unique(this)

    Given a collection, create a list with all duplicates removed

Methods for `MemberReference`
"""""""""""""""""""""""""""""
.. method:: MemberReference.doc(this)

    Given any object, return the documentation associated with it

.. method:: MemberReference.help(this)

    Print formatted help for the given object

.. method:: MemberReference.img(this)

    Return a string representation of an object

.. method:: MemberReference.print(this)

    Built-in print function. Prints the argument

Methods for `Namespace`
"""""""""""""""""""""""
.. method:: Namespace.doc(this)

    Given any object, return the documentation associated with it

.. method:: Namespace.help(this)

    Print formatted help for the given object

.. method:: Namespace.img(this)

    Return a string representation of an object

.. method:: Namespace.print(this)

    Built-in print function. Prints the argument

Methods for `Node`
""""""""""""""""""
.. method:: Node.children(this)

    Return the node's children

.. method:: Node.children_count(this)

    Return the node's children count

.. method:: Node.doc(this)

    Given any object, return the documentation associated with it

.. method:: Node.dump(this)

    Dump the node's content in a structured tree

.. method:: Node.help(this)

    Print formatted help for the given object

.. method:: Node.image(this)

    Return the node's image

.. method:: Node.img(this)

    Return a string representation of an object

.. method:: Node.kind(this)

    Return the node's kind

.. method:: Node.parent(this)

    Return the node's parent

.. method:: Node.print(this)

    Built-in print function. Prints the argument

.. method:: Node.same_tokens(this, right_node)

    Return whether two nodes have the same tokens, ignoring trivias

.. method:: Node.text(this)

    Return the node's text

.. method:: Node.tokens(this)

    Return the node's tokens

.. method:: Node.unit(this)

    Return the node's analysis unit

Methods for `Object`
""""""""""""""""""""
.. method:: Object.combine(this, right, recursive)

    Combine two LKQL values if possible and return the result, recursively if required

.. method:: Object.doc(this)

    Given any object, return the documentation associated with it

.. method:: Object.help(this)

    Print formatted help for the given object

.. method:: Object.img(this)

    Return a string representation of an object

.. method:: Object.print(this)

    Built-in print function. Prints the argument

Methods for `Pattern`
"""""""""""""""""""""
.. method:: Pattern.doc(this)

    Given any object, return the documentation associated with it

.. method:: Pattern.help(this)

    Print formatted help for the given object

.. method:: Pattern.img(this)

    Return a string representation of an object

.. method:: Pattern.print(this)

    Built-in print function. Prints the argument

Methods for `PropertyReference`
"""""""""""""""""""""""""""""""
.. method:: PropertyReference.doc(this)

    Given any object, return the documentation associated with it

.. method:: PropertyReference.help(this)

    Print formatted help for the given object

.. method:: PropertyReference.img(this)

    Return a string representation of an object

.. method:: PropertyReference.print(this)

    Built-in print function. Prints the argument

Methods for `RecValue`
""""""""""""""""""""""
.. method:: RecValue.doc(this)

    Given any object, return the documentation associated with it

.. method:: RecValue.help(this)

    Print formatted help for the given object

.. method:: RecValue.img(this)

    Return a string representation of an object

.. method:: RecValue.print(this)

    Built-in print function. Prints the argument

Methods for `RewritingContext`
""""""""""""""""""""""""""""""
.. method:: RewritingContext.add_first(this, node, new_node)

    Insert `new_node` at the beginning of `list_node`

.. method:: RewritingContext.add_last(this, node, new_node)

    Insert `new_node` at the end of `list_node`

.. method:: RewritingContext.create_from_template(this, template, grammar_rule, arguments)

    Create a new node from the provided template, filling '{}' with provided
    argument, and parsing the template with the specified grammar rule. Example:
    
    .. code-block:: lkql
    
      # Create a new BinOp node with OpAdd as operator, representing the addition of the value
      # expressed by `my_other_node`, and "42".
      ctx.create_from_template(
          "{} + 42",
          "expr_rule",
          [my_other_node]
      )
    

.. method:: RewritingContext.doc(this)

    Given any object, return the documentation associated with it

.. method:: RewritingContext.help(this)

    Print formatted help for the given object

.. method:: RewritingContext.img(this)

    Return a string representation of an object

.. method:: RewritingContext.insert_after(this, node, new_node)

    Insert `new_node` after `node` (`node`'s parent needs to be a list node)

.. method:: RewritingContext.insert_before(this, node, new_node)

    Insert `new_node` before `node` (`node`'s parent needs to be a list node)

.. method:: RewritingContext.print(this)

    Built-in print function. Prints the argument

.. method:: RewritingContext.remove(this, obj_to_remove)

    Delete the given node from its parent (parent needs to be a list node)

.. method:: RewritingContext.replace(this, old_node, new_node)

    Replace old node by the new one

.. method:: RewritingContext.set_child(this, node, member_ref, new_value)

    Set the node child, following the given member reference, to the new value

Methods for `RewritingNode`
"""""""""""""""""""""""""""
.. method:: RewritingNode.clone(this)

    Given a rewriting node, clone it and return its copy

.. method:: RewritingNode.doc(this)

    Given any object, return the documentation associated with it

.. method:: RewritingNode.help(this)

    Print formatted help for the given object

.. method:: RewritingNode.img(this)

    Return a string representation of an object

.. method:: RewritingNode.print(this)

    Built-in print function. Prints the argument

Methods for `Selector`
""""""""""""""""""""""
.. method:: Selector.doc(this)

    Given any object, return the documentation associated with it

.. method:: Selector.help(this)

    Print formatted help for the given object

.. method:: Selector.img(this)

    Return a string representation of an object

.. method:: Selector.print(this)

    Built-in print function. Prints the argument

Methods for `SelectorList`
""""""""""""""""""""""""""
.. method:: SelectorList.doc(this)

    Given any object, return the documentation associated with it

.. method:: SelectorList.enumerate(this)

    Return the content of the iterable object with each element associated to its index in a tuple: [(<index>, <elem>), ...]

.. method:: SelectorList.help(this)

    Print formatted help for the given object

.. method:: SelectorList.img(this)

    Return a string representation of an object

.. method:: SelectorList.length(this)

    Return the length of the iterable

.. method:: SelectorList.print(this)

    Built-in print function. Prints the argument

.. method:: SelectorList.reduce(this, function, init_value)

    Given a collection, a reduction function, and an initial value reduce the result

.. method:: SelectorList.to_list(this)

    Transform into a list

Methods for `Str`
"""""""""""""""""
.. method:: Str.base_name(this)

    Given a string that represents a file name, returns the basename

.. method:: Str.combine(this, right, recursive)

    Combine two LKQL values if possible and return the result, recursively if required

.. method:: Str.contains(this, to_find)

    Search for to_find in the given string. Return whether a match is found. to_find can be either a pattern or a string

.. method:: Str.doc(this)

    Given any object, return the documentation associated with it

.. method:: Str.ends_with(this, suffix)

    Returns whether string ends with given prefix

.. method:: Str.find(this, to_find)

    Search for to_find in the given string. Return position of the match, or -1 if no match. to_find can be either a pattern or a string

.. method:: Str.help(this)

    Print formatted help for the given object

.. method:: Str.img(this)

    Return a string representation of an object

.. method:: Str.is_lower_case(this)

    Return whether the string is in lowercase

.. method:: Str.is_mixed_case(this)

    Return whether the given string is written in mixed case, that is, with only lower case characters except the first one and every character following an underscore

.. method:: Str.is_upper_case(this)

    Return whether the string is in uppercase

.. method:: Str.length(this)

    Return the string's length

.. method:: Str.print(this)

    Built-in print function. Prints the argument

.. method:: Str.split(this, sep)

    Given a string, split it on the given separator, and return an iterator on the parts

.. method:: Str.starts_with(this, prefix)

    Returns whether string starts with given prefix

.. method:: Str.substring(this, start, end)

    Given a string and two indices (from and to), return the substring contained between indices from and to (both included)

.. method:: Str.to_lower_case(this)

    Return the string in lowercase

.. method:: Str.to_upper_case(this)

    Return the string in uppercase

Methods for `Token`
"""""""""""""""""""
.. method:: Token.doc(this)

    Given any object, return the documentation associated with it

.. method:: Token.end_column(this)

    Return the end column

.. method:: Token.end_line(this)

    Return the end line

.. method:: Token.help(this)

    Print formatted help for the given object

.. method:: Token.img(this)

    Return a string representation of an object

.. method:: Token.is_equivalent(this, other)

    Return whether two tokens are structurally equivalent

.. method:: Token.is_trivia(this)

    Return whether this token is a trivia

.. method:: Token.kind(this)

    Return the kind for this token

.. method:: Token.next(this, ignore_trivia)

    Return the next token

.. method:: Token.previous(this, exclude_trivia)

    Return the previous token

.. method:: Token.print(this)

    Built-in print function. Prints the argument

.. method:: Token.start_column(this)

    Return the start column

.. method:: Token.start_line(this)

    Return the start line

.. method:: Token.text(this)

    Return the text for this token

.. method:: Token.unit(this)

    Return the unit for this token

Methods for `Tuple`
"""""""""""""""""""
.. method:: Tuple.doc(this)

    Given any object, return the documentation associated with it

.. method:: Tuple.help(this)

    Print formatted help for the given object

.. method:: Tuple.img(this)

    Return a string representation of an object

.. method:: Tuple.print(this)

    Built-in print function. Prints the argument

Methods for `Unit`
""""""""""""""""""
.. method:: Unit.doc(this)

    Given any object, return the documentation associated with it

.. method:: Unit.help(this)

    Print formatted help for the given object

.. method:: Unit.img(this)

    Return a string representation of an object

.. method:: Unit.print(this)

    Built-in print function. Prints the argument

