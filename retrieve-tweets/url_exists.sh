#!/bin/bash

for url in $(cat datafiles/spanish_urls) ; do
    if curl --output /dev/null --silent --head --fail "$url"
    then
	echo "$url exists"
    else
	echo "$url does not exist"
    fi
done

## Code taken from Charles Duffy's answer: https://stackoverflow.com/questions/12199059/how-to-check-if-an-url-exists-with-the-shell-and-probably-curl
