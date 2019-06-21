# exit on error
set -e
git clean -fdx
npm install
git push
vsce publish
