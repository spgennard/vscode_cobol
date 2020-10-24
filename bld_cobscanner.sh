set -e

test ! -d cobscanner && mkdir cobscanner
cp ./out/cobscannerdata.js cobscanner/
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
