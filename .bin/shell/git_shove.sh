#!/bin/bash
# quickly push changes to a github repo
set -e
git add .
git commit -a -m "autoupdate"
git push origin master 
