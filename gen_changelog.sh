# exit on error
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")

echo "# CHANGELOG" >CHANGELOG.md
echo >>CHANGELOG.md
git log --oneline --decorate $PACKAGE_VERSION | xargs -d\\n -n 1 echo '*' >>CHANGELOG.md

