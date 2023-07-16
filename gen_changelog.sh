#!/bin/bash

# exit on error
set -e
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
MSG="Update CHANGELOG.md"
git tag -f $PACKAGE_VERSION
git push --tags --force

echo "# CHANGELOG" >CHANGELOG.md
echo >>CHANGELOG.md
OLDIFS=$IFS
git log --oneline --decorate $PACKAGE_VERSION | while read i 
do
	set -- $i
    case "$2" in
        "(tag:"*|"(HEAD"*) 
            echo
		    X=$(echo "$i" | sed "s/.*tag: //g" | sed "s/)//g" | sed "s/,.*//" | sed 's/ .*//')
            echo "## $X"
		    echo
        ;;
        *) echo "* $i" ;;
    esac
done | grep -v "$MSG" | 
	grep -v "bump" | 
	grep -v "update$" | 
	sed "/^$/d" |
	sed "s/^##/\n##/" >>CHANGELOG.md
IFS=$OLDIFS


git commit -m $MSG CHANGELOG.md && true
git push
git tag -f $PACKAGE_VERSION
git push --tags --force


