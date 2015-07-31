#!/bin/bash

# Syntax:
# ./generate_url.sh <filename> <function_name>
#
# This will generate a GitHub URL which can be utilized
# while linking to a particular function

# Check for argument consistency
if [ $# -ne 2 ]; then
	echo "Number of arguments != 2"
	exit 1
fi

# Check for file existence
if [ ! -f $1 ]; then
	echo "File not found: $1"
	exit 2
fi

# Fetch the commit hash
COMMIT_HASH=`git log -1 --pretty=format:%H -- $1`

# Check for non-zero commit hash
if [ -z "$COMMIT_HASH" ]; then
	echo "Invalid Commit Hash: $COMMIT_HASH"
	exit 3
fi

# Now finally search for the line number in that definition
LINE_START=`grep -n -R "(define[[:space:]]\{1,\}(${2}[[:space:]]\{1,\}" $1  \
		| cut -d':' -f1`

# Now generate the URL
URL="blob/${COMMIT_HASH}/${1}#L${LINE_START}"
echo $URL

# TODO: Even though I can find all the matching brackets present after
# the start of function, I'm not able to segregate only one match
#
# Hint: grep -zPo --max-count=1 '(\(([^()]++|(?1))*\))' with tail-n+<lineno>
