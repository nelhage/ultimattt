#!/bin/bash
./flatten.sh "@" \
    | sort | uniq -c | sort -rnk 1,1
