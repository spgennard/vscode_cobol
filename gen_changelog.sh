#!/bin/bash

# exit on error
set -e
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
MSG="Update CHANGELOG.md"
git tag -f $PACKAGE_VERSION
git push --tags --force

echo "# CHANGELOG" >CHANGELOG.md
echo >>CHANGELOG.md
for i in $(git log --oneline --decorate $PACKAGE_VERSION) 
do
	echo "* $i" | grep -v "$MSG" | grep -v "bump" | grep -v -E "update$" >>CHANGELOG.md
done


git commit -m $MSG CHANGELOG.md && true
git push
git tag -f $PACKAGE_VERSION
git push --tags --force


