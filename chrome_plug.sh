#!/bin/sh

printf "Chromium version:" >&2
read -r CHROMIUM_VERSION

printf "Chrome Plugin id:" >&2
read -r CHROME_PLUGIN_ID

echo "Installing Plugin $CHROME_PLUGIN_ID for Chromium version $CHROMIUM_VERSION..."

CHROME_PLUGIN_DOWNLOAD="chrome_plugin_$CHROME_PLUGIN_ID.crx"

curl -L "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=$CHROMIUM_VERSION&x=id%3D$CHROME_PLUGIN_ID%26installsource%3Dondemand%26uc" > "$CHROME_PLUGIN_DOWNLOAD"

echo "Check installation at $(pwd)/$CHROME_PLUGIN_DOWNLOAD"
