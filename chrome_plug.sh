#!/bin/sh

printf "Chromium version:" >&2
read -r chromium_version

printf "Chrome Plugin id:" >&2
read -r chrome_plugin_id

echo "Installing Plugin $chrome_plugin_id for Chromium version $chromium_version..."

chrome_plugin_download="chrome_plugin_$chrome_plugin_id.crx"

dir_downloads="$HOME/Downloads"
if [ -d "$dir_downloads" ]
then
    cd "$dir_downloads" || echo "Failed to change directory: $dir_downloads" && exit 1
fi

curl -L "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=$chromium_version&x=id%3D$chrome_plugin_id%26installsource%3Dondemand%26uc" > "$chrome_plugin_download"

echo "Check installation at $(pwd)/$chrome_plugin_download"
