# exit on error
set -e
echo "TIP can use : (git tag -l --sort=refname 3.*)"
for i in $*
do
	PACKAGE_VERSION=$1
	github-release delete \
	  --owner=spgennard \
	  --repo=vscode_cobol \
	  --tag="$PACKAGE_VERSION" \
	  --name=$PACKAGE_VERSION
	shift
done
