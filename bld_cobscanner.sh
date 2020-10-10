set -e

test ! -d cobscanner && mkdir cobscanner
cp ./out/consoleexternalfeatures.js cobscanner/
cp ./out/cobolglobalcache_file.js cobscanner/
cp ./out/globalcachehelper.js cobscanner/
cp ./out/cobolsymboltableeventhelper.js cobscanner/
cp ./out/iconfiguration.js cobscanner/
cp ./out/externalfeatures.js cobscanner/
cp ./out/filesourcehandler.js cobscanner/
cp ./out/cobolglobalcache.js cobscanner/
cp ./out/cobscanner.js cobscanner/
cp ./out/cobolsourcescanner.js cobscanner/
cp -r ./out/keywords cobscanner/

PACKAGE_VERSION=$(node -p -e "require('./package.json').version")

cd cobscanner
pkg -t "node10-linux-x64,node10-macos-x64,node10-win-x64" cobscanner.js

test -f cobscanner-linux-* && rm -f cobscanner-linux-*
test -f cobscanner-macos-* && rm -f cobscanner-macos-*
test -f cobscanner-linux-* && rm -f cobscanner-win*.exe

mv cobscanner-linux cobscanner-linux-${PACKAGE_VERSION}
mv cobscanner-macos cobscanner-macos-${PACKAGE_VERSION}
mv cobscanner-win.exe cobscanner-win-${PACKAGE_VERSION}.exe
cd ..

test ! -d bin && mkdir bin
mv cobscanner/cobscanner-* bin/
rm -rf cobscanner
