#!/bin/sh
set -e

#Handy script to deploy demo-app to gh-pages

# get comment
comment="deploy gh-pages"

scriptFile=$(readlink -f "$0")
projectPath=$(dirname "$scriptFile")/..

cd $projectPath

git checkout master
git pull
git checkout gh-pages
git merge
git merge master

sbt clean

sbt "demo/fullOptJS"

ghPagesDir=$projectPath/"gh-pages"

mkdir -p ${ghPagesDir}

cp demo/index.html $projectPath

cp demo/build/demo-jsdeps.js ${ghPagesDir}
cp demo/build/demo-opt.js ${ghPagesDir}
cp demo/style.css ${ghPagesDir}

git add ${projectPath}

git commit -m "$comment"

echo "all you need to do is to 'git push'"
