# exit on error
set -e

PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
git tag -f $PACKAGE_VERSION
git push --tags --force

./gen_changelog.sh

[ -d ".vscode_test" ] && cp -r .vscode-test ..
git clean -fdx
npm install
git push
#npm run beforepublish
vsce publish
#npm run afterpublish
rm -f *.vsix

mkdir .vscode-test 2>/dev/null && true
[ -d "../.vscode_test" ] && cp -r ../.vscode-test .

npm-check-updates

echo "use: ncu -u"
