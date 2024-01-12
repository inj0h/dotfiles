#!/bin/sh

color_red="\033[0;31m"
color_green="\033[0;32m"

# Where \033[0m is ASCII for clearing the color code
print_color() { printf "$2%s\033[0m" "$1" ; }

exit_cd()
{
    print_color "$color_red" "ERROR: "
    printf     "Failed to change directory - "
    print_color "$color_red" "$1"
    echo
    exit 1
}

printf "Chromium version:" >&2
read -r chromium_version

printf "Chrome Plugin Id:" >&2
read -r chrome_plugin_id

echo "Installing Plugin $chrome_plugin_id for Chromium version $chromium_version..."

dir_downloads="$HOME/Downloads"
cd "$dir_downloads" || exit_cd "$dir_downloads"

chrome_plugin_download="chrome_plugin_$chrome_plugin_id.crx"
curl -L "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=$chromium_version&x=id%3D$chrome_plugin_id%26installsource%3Dondemand%26uc" > "$chrome_plugin_download"

printf "Check installation at "
print_color "$color_green" "$(pwd)/$chrome_plugin_download"
echo
