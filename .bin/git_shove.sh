#!/bin/bash
set -e
git add .
git commit -a -m "autoupdate"
git push origin master
