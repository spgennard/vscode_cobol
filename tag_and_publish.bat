git clean -fdx
@if errorlevel 1 goto theend

call npm install
@if errorlevel 1 goto theend

call npm version patch
@if errorlevel 1 goto theend

git push
@if errorlevel 1 goto theend

git push --tags
@if errorlevel 1 goto theend

vsce publish
@if errorlevel 1 goto theend


:theend