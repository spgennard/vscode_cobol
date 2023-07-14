#!/bin/bash

# exit on error
set -e
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
MSG="Update CHANGELOG.md"
git tag -f $PACKAGE_VERSION
git push --tags --force

echo "# CHANGELOG" >CHANGELOG.md
echo >>CHANGELOG.md
git log --oneline --decorate $PACKAGE_VERSION | while read i 
do
	echo "* $i"
done | grep -v "$MSG" | grep -v "bump" | grep -v "update$" >>CHANGELOG.md


git commit -m $MSG CHANGELOG.md && true
git push
git tag -f $PACKAGE_VERSION
git push --tags --force


