# exit on error
set -e
git clean -fdx
call npm install
git push
vsce publish
