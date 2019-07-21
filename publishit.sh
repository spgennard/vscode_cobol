# exit on error
set -e
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
  --tag="latest" \
  --name=$PACKAGE_VERSION \
  --body="${COMMIT_LOG}" \
  cobol*.vsix
