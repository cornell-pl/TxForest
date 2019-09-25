#!/bin/bash

dune build;
if [ $? -eq 0 ];
then
    for file in $(find _build -name *.exe | grep -v ppx);
    do  echo "$(basename $file):";
        $file "richard" "jonathan" 100
    done
else
    echo "COMPILATION FAILED"
fi
