#!/bin/bash

for url in $(cat datafiles/italian_urls) ; do
    if curl --output /dev/null --silent --head --fail "$url"
    then
	echo "$url exists"
    else
	echo "$url does not exist"
    fi
done
