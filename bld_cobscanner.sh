set +e
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")

export PATH=$(pwd)/./node_modules/.bin:$PATH
#browserify out/cobscanner.js --require n-readlines --outfile cobscanner/cobscanner.js
cp ./out/cobscanner.js cobscanner/
cp ./out/cobscannerdata.js cobscanner/
cp ./out/consoleexternalfeatures.js cobscanner/
cp ./out/cobolglobalcache_file.js cobscanner/
cp ./out/globalcachehelper.js cobscanner/
cp ./out/cobolsymboltableeventhelper.js cobscanner/
cp ./out/iconfiguration.js cobscanner/
cp ./out/externalfeatures.js cobscanner/
cp ./out/filesourcehandler.js cobscanner/
cp ./out/cobolglobalcache.js cobscanner/
cp ./out/cobolsourcescanner.js cobscanner/
cp -r ./out/keywords cobscanner/
cp -r ./out/keywords cobscanner/
cd cobscanner
npm version --allow-same-version $PACKAGE_VERSION
npm install
