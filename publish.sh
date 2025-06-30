# exit on error
VER=$(date +'%y.%-m.%-d')
for i in $@
do
	echo i is $i
	if [ "x$i" == "xyesterday" ]; then
		VER=$(date -d "yesterday" +'%2y.%-m.%-d')
	elif [ "x$i" == "xnobump" ]; then
		VER=""
	fi
done

if [ ! "x$VER" = "x" ]; then
	npm version --no-git-tag-version $VER
	if [ $? -ne 0 ]; then
       		exit 1
	fi
	git commit -m "bump" package.json
	git push
fi
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
git tag -f $PACKAGE_VERSION
git push --tags --force

./gen_changelog.sh

echo Updating

[ -d ".vscode_test" ] && cp -r .vscode-test ..
git clean -fdx
./get_dotfiles.sh
npm install
git push
#npm run beforepublish
vsce publish
#npm run afterpublish
rm -f *.vsix

mkdir .vscode-test 2>/dev/null && true
[ -d "../.vscode_test" ] && cp -r ../.vscode-test .

./node_modules/.bin/ncu


echo "use: ncu -u"
