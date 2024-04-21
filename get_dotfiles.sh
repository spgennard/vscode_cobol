TOP=$(pwd)
mkdir dotfiles
mkdir tmp
cd tmp
git clone --depth 1 git@github.com:spgennard/dotfiles.git 
cd dotfiles
rm -rf .git
cp bin/scan4install.* $TOP/dotfiles/
cd ../..
pwd
rm -rf tmp/


