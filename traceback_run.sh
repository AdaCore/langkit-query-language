#!/bin/bash

# Display the stack trace when a program's execution ends with an unhandled 
# exception.
#
# In order for this script to work, the following lines must be added to the GPR file:
#   
#   package Binder is
#       for Default_Switches ("Ada") use Binder'Default_Switches ("Ada") & ("-E");
#   end Binder;

OUTPUT="$($@ 2>&1)"

ADDRESSES="$(echo "$OUTPUT" | grep '0x')"
PROG_OUTPUT="$(echo "$OUTPUT" | grep -v '0x')"

echo "$PROG_OUTPUT"

[ ! -z $ADDRESSES ] && 
    echo "$(addr2line --exe=$1 --demangle=gnat $ADDRESSES | grep -v ??:)"