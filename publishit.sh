# exit on error
set -e
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
git tag $PACKAGE_VERSION
git push --tags
git clean -fdx
npm install
git push
vsce publish
rm -f *.vsix
vsce package
COMMIT_LOG=$(git log -1 --format='%ci %H %s')
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
github-release upload \
  --owner=spgennard \
  --repo=vscode_cobol \
  --tag="$PACKAGE_VERSION" \
  --name=$PACKAGE_VERSION \
  --body="${COMMIT_LOG}" \
  cobol*.vsix


npm-check-updates

echo "use: ncu -u"
