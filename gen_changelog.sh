# exit on error
set -e
set -x
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
MSG="Update CHANGELOG.md"

echo "# CHANGELOG" >CHANGELOG.md
echo >>CHANGELOG.md
git log --oneline --decorate $PACKAGE_VERSION | xargs -d\\n -n 1 echo '*' | grep -v "$MSG" | grep -v "bump" | grep -v -E "update$" >>CHANGELOG.md

git commit -m $MSG CHANGELOG.md && true
git push
git tag -f $PACKAGE_VERSION
git push --tags --force


