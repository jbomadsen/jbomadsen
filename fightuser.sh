#!/usr/bin/env bash
# TRON profile -> SVG visualization
{ echo 'digraph { begin -> '; <"$1" sed -e 's,\[,",g' -e 's,\],"->,g'; echo 'end ; }'; }  | dot -Tsvg -o fightuser.svg
