#!/bin/bash
#
# filename:         git-shove.sh
# description:
#                   Add and commit changes. Pollute commit log. Force push.
#                   Take care to only use this script when in a pinch.
#
# ---------------------------------------------------------------------------- #

set -e

git add .
git commit -am "Emergency push!"
git push
