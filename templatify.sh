#!/usr/bin/env bash

echo "Moving templatified sources to lkql/lkql_src_templates..."

cp src/ lkql/lkql_src_templates
cp lkql_repl lkql/lkql_src_templates

sed -i 's/LAL.Ada_Node/LAL.${root_name}/g' lkql_src_templates/*
sed -i 's/No_Ada_Node/No_${root_name}/g' lkql/src_templates/*
