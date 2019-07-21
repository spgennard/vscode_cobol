# exit on error
set -e
git clean -fdx
npm install
git push
vsce publish
rm -f *.vsix
vsce package
COMMIT_LOG=$(git log -1 --format='%ci %H %s')
github-release upload \
  --owner=spgennard \
  --repo=vscode_cobol \
  --tag="latest" \
  --body="${COMMIT_LOG}" \
  cobol*.vsix
