#! /bin/bash

if [ -e /etc/mime.types ]
then 
    echo "\`(" && 
    	# $3 != ""
    cat /etc/mime.types | awk '{if ($2 != "" && $1 != "#")  { print "(" $1 " (" $2 " "$3 " " $4 " " $5 " " $6 " " $7 " " $8 " " $9 " " $10 "))" };}' | sed "/#/d" && 
    echo ")"; 
else 
    echo "You don't have debian mime-support!\n"; 
fi
