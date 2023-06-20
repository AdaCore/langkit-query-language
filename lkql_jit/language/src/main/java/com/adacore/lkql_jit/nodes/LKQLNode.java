/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.LKQLTypeSystem;
import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_functions.ReflectionUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;


/**
 * The base node of the LKQL implementation in Truffle, all other nodes come from it.
 *
 * @author Hugo GUERRIER
 */
@TypeSystemReference(LKQLTypeSystem.class)
public abstract class LKQLNode extends Node implements Locatable {

    // ----- Attributes -----

    /**
     * The location of the node in the source.
     */
    protected final SourceLocation location;

    // ----- Constructor -----

    /**
     * Base constructor for all LKQL nodes.
     *
     * @param location The location of the node in the source.
     */
    protected LKQLNode(
        SourceLocation location
    ) {
        this.location = location;
    }

    // ----- Getters -----

    /**
     * @see com.adacore.lkql_jit.utils.source_location.Locatable#getLocation()
     */
    @Override
    public SourceLocation getLocation() {
        return location;
    }

    // ----- Execution methods -----

    /**
     * Execute the node as a generic value, it returns an untyped java object.
     *
     * @param frame The frame to execute the node in.
     * @return The result of the node execution wrapped in an object.
     */
    public abstract Object executeGeneric(VirtualFrame frame);

    // ----- Instance methods -----

    /**
     * Create the string representation of the node with its children.
     *
     * @param indentLevel The level of indentation.
     * @return The string representation of the node.
     */
    protected String nodeRepresentation(int indentLevel) {
        return this.nodeRepresentation(indentLevel, new String[0], new Object[0]);
    }

    /**
     * Create a node string representation with its name and fields, THIS AUTOMATICALLY INCLUDE NODE'S CHILDREN.
     *
     * @param indentLevel The level of indentation.
     * @param fields      The field names.
     * @param values      The field values.
     * @return The string representation of the node.
     */
    @CompilerDirectives.TruffleBoundary
    protected String nodeRepresentation(int indentLevel, String[] fields, Object[] values) {
        // Prepare the string builder
        StringBuilder builder = new StringBuilder();
        builder.append(this.getClass().getSimpleName()).append('{');

        // Get the children representation
        List<String> childrenRepresentation = this.getChildrenRepresentations(indentLevel);

        // If the fields and children are empty, just return an empty node
        if (fields.length == 0 && childrenRepresentation.size() == 0) {
            builder.append('}');
            return builder.toString();
        } else {
            builder.append('\n');
        }

        // Add the node custom fields
        for (int i = 0; i < fields.length && i < values.length; i++) {
            this.indent(indentLevel, builder);
            builder.append(fields[i])
                .append(" = ")
                .append(values[i] == null ? "null" : values[i].toString())
                .append('\n');
        }

        // Add the node children
        for (String child : childrenRepresentation) {
            this.indent(indentLevel, builder);
            builder.append(child).append('\n');
        }

        // Finish the node
        this.indent(--indentLevel, builder);
        builder.append("}");

        // Return the result by building the string
        return builder.toString();
    }

    /**
     * Indent the builder with the correct level.
     *
     * @param indentLevel The indent level.
     * @param builder     The builder to indent.
     */
    private void indent(int indentLevel, StringBuilder builder) {
        builder.append("   ".repeat(Math.max(0, indentLevel)));
    }

    /**
     * Get a list containing the node children representation associated with their name.
     *
     * @param indentLevel The level of indentation of children nodes.
     * @return The list of children representation.
     */
    private List<String> getChildrenRepresentations(int indentLevel) {
        // Prepare the result
        List<String> res = new ArrayList<>();

        // Get the fields annotated with child or children
        List<Field> childFields = ReflectionUtils.getFieldsUpTo(this.getClass(), null)
            .stream().filter(
                field -> field.getAnnotation(Child.class) != null || field.getAnnotation(Children.class) != null
            ).toList();

        // Add all field to the result
        for (Field childField : childFields) {
            try {
                // Get the field value
                childField.setAccessible(true);
                Object fieldValue = childField.get(this);
                childField.setAccessible(false);

                // Transform the value to a string
                String fieldValueString;

                // If the value is null
                if (fieldValue == null) {
                    fieldValueString = "null";
                }

                // If the value is an array of node
                else if (fieldValue.getClass().isArray()) {
                    LKQLNode[] nodeArray = (LKQLNode[]) fieldValue;

                    // If the node array is empty
                    if (nodeArray.length == 0) {
                        fieldValueString = "[]";
                    }

                    // If the node array is not empty
                    else {
                        // Create a string builder for the nodes
                        StringBuilder nodeArrayStringBuilder = new StringBuilder();
                        nodeArrayStringBuilder.append("[\n");
                        indentLevel++;

                        // Add all node in the string builder
                        for (int i = 0; i < nodeArray.length; i++) {
                            this.indent(indentLevel, nodeArrayStringBuilder);
                            nodeArrayStringBuilder.append(nodeArray[i].toString(indentLevel + 1));
                            if (i < nodeArray.length - 1) {
                                nodeArrayStringBuilder.append(",");
                            }
                            nodeArrayStringBuilder.append('\n');
                        }

                        // Finish the string builder
                        this.indent(--indentLevel, nodeArrayStringBuilder);
                        nodeArrayStringBuilder.append(']');
                        fieldValueString = nodeArrayStringBuilder.toString();
                    }
                }

                // If the field is a LKQL node
                else if (fieldValue instanceof LKQLNode lkqlNode) {
                    fieldValueString = lkqlNode.toString(indentLevel + 1);
                }

                // Else if the field is just a node
                else {
                    fieldValueString = fieldValue.toString();
                }

                // Add the string to the result
                res.add(childField.getName() + " = " + fieldValueString);
            } catch (Exception e) {
                System.err.println("Cannot get the child " + childField.getName() + " for " + this.getClass().getSimpleName());
                e.printStackTrace();
            }
        }

        // Return the result
        return res;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.toString(1);
    }

    /**
     * Get the tree representation of the node in a string.
     *
     * @param indentLevel The indent level of this node.
     * @return The tree representation of the node in a string.
     */
    public abstract String toString(int indentLevel);

}
