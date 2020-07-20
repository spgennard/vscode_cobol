# exit on error
set -e
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")

npx git-changelog-command-line --ignore-commits-without-issue -std -tec "
# Changelog

Changelog for {{ownerName}} {{repoName}}.

{{#tags}}
## {{name}}
 {{#issues}}
  {{#hasIssue}}
   {{#hasLink}}
### {{name}} [{{issue}}]({{link}}) {{title}} {{#hasIssueType}} *{{issueType}}* {{/hasIssueType}} {{#hasLabels}} {{#labels}} *{{.}}* {{/labels}} {{/hasLabels}}
   {{/hasLink}}
   {{^hasLink}}
### {{name}} {{issue}} {{title}} {{#hasIssueType}} *{{issueType}}* {{/hasIssueType}} {{#hasLabels}} {{#labels}} *{{.}}* {{/labels}} {{/hasLabels}}
   {{/hasLink}}
  {{/hasIssue}}
  {{^hasIssue}}
### {{name}}
  {{/hasIssue}}

  {{#commits}}
**{{{messageTitle}}}**

{{#messageBodyItems}}
 * {{.}} 
{{/messageBodyItems}}

[{{hash}}](https://github.com/{{ownerName}}/{{repoName}}/commit/{{hash}}) {{authorName}} *{{commitTime}}*

  {{/commits}}

 {{/issues}}
{{/tags}}
" >CHANGELOG.md
git commit -m "Update CHANGELOG.md" CHANGELOG.md
git push
git tag $PACKAGE_VERSION
git push --tags
git clean -fdx
npm install
git push
vsce publish
rm -f *.vsix
vsce package
COMMIT_LOG=$(git log -1 --format='%ci %H %s')
PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
github-release upload \
  --owner=spgennard \
  --repo=vscode_cobol \
  --tag="$PACKAGE_VERSION" \
  --name=$PACKAGE_VERSION \
  --body="${COMMIT_LOG}" \
  cobol*.vsix

npx ovsx publish cobol*.vsix -p $(cat $HOME/.ovsx.token)

npm-check-updates

echo "use: ncu -u"
