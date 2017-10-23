#!/bin/bash
# quickly push changes to a github repo
set -e
git add .
git commit -am "Emergency push!"
git push origin master
