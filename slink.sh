#!/bin/sh

# A POSIX shell script to create and destroy symbolic links for a set of files
# outlined by an argument configuration file.


# Variables: #


# Arguments:
flag_help="--help"
flag_help_abbrev="-h"
flag_create="--create"
flag_create_abbrev="-c"
flag_delete="--delete"
flag_delete_abbrev="-d"

# Printing:
color_red="\033[0;31m"
color_green="\033[0;32m"
color_yellow="\033[0;33m"
format_bold="\033[1m"
format_underline="\033[4m"

# Etc:
config_comment="#"


# Functions: #


# Used by other functions:

print_color()  { printf "$1%s\033[0m" "$2"        ; }
print_green()  { print_color "$color_green" "$1"  ; }
print_red()    { print_color "$color_red" "$1"    ; }
print_yellow() { print_color "$color_yellow" "$1" ; }


config_validate()
{
    while IFS="," read -r filetype filepath linkpath
    do
        filepath_full="$HOME/$filepath"
        linkpath_directory="$HOME/$(dirname "$linkpath")"
        case "$filetype" in
            "d"*)
                if [ ! -d "$filepath_full" ]
                then
                    print_error "Error:     Failed to find directory." \
                                "Directory: $filepath_full" \
                                "No links created."
                fi
                ;;
            "f"*)
                if [ ! -r "$filepath_full" ]
                then
                    print_error "Error: Failed to find or read file." \
                                "File:  $filepath_full" \
                                "No links created."
                fi
                ;;
            "" | "$config_comment"*) # Skip blank lines and comments
                true
                ;;
            *)
                print_error "Error:   Non-parsable content detected!" \
                            "Content: $filetype" \
                            "No links created."
        esac
        # Check whether any intermediate directories for the target exist.
        # E.g.  Source = ~/foo/test.txt
        #       Target = ~/bar/baz/test.txt
        #                      ^
        #                      |
        #                      +--- Doesn't actually exist.
        if [ ! -d "$linkpath_directory" ]
        then
            print_error "Error:     Failed to find directory." \
                        "Directory: $linkpath_directory" \
                        "No links created."
        fi
    done < "$1"
}

link_create()
{
    # This function assumes that the supplied configuration file exists and
    # that config_validate can parse its contents.
    case "$(uname -s)" in
        "Darwin")
            print_yellow "BSD / Mac system detected." ; echo
            echo "Setting flags..."
            ln_flags="-shfv"
            ;;
        "Linux")
            print_yellow "Linux system detected." ; echo
            echo "Setting flags..."
            ln_flags="-sTfv"
            ;;
        *)
            print_red "Error: Incompatible system detected." ; echo
            echo "Exiting with nonzero code..."
            exit 1
    esac
    echo "Done."
    echo "Linking..."
    while IFS="," read -r filetype filepath linkpath
    do
        case "$filetype" in
            "" | "$config_comment"*) # Skip blank lines and comments
                true
                ;;
            *)
                filepath_full="$HOME/$filepath"
                linkpath_full="$HOME/$linkpath"
                ln "$ln_flags" "$filepath_full" "$linkpath_full"
        esac
    done < "$1"
    print_green "Done." ; echo
}

link_delete()
{
    # This function assumes that the supplied configuration file exists and
    # that config_validate can parse its contents.
    echo "Delinking..."
    while IFS="," read -r filetype filepath linkpath
    do
        case "$filetype" in
            "" | "$config_comment"*) # Skip blank lines and comments
                true
                ;;
            *)
                linkpath_full="$HOME/$linkpath"
                linkpath_full_noexpand="${HOME:?}/$linkpath"
                rm -rf "$linkpath_full_noexpand"
                echo "-> $linkpath_full"
        esac
    done < "$1"
    print_green "Done." ; echo
}

print_header()
{
    printf "$format_bold$format_underline%s\033[0m" "$1" ;
}

print_help()
{
    print_header "Usage:"
    echo " slink.sh [options] [file]" ; echo
    print_header "Options:" ; echo
    echo "$flag_help_abbrev, $flag_help
Print help, i.e. this output.

$flag_create_abbrev, $flag_create [file]
Parse the contents of the argument configuration file, and if it contains no
errors, i.e. the relevant directories and files exist, create symbolic links
for the file(s) listed per line.

$flag_delete_abbrev, $flag_delete [file]
Parse the contents of the argument configuration file, and if it contains no
errors, i.e. the relevant directories and files exist, delete the symbolic
links for the file(s) listed per line.

The argument configuration file should have the following format.

filetype + , + path_to_file + , + path_to_symbolic_link

The filetype specifies either a directory, specified by the character \"d\", or
a file, specified by the character \"f\".

Paths assume the home directory as the topmost directory. Therefore, any
provided paths will have \"~/\" prefixed before them.

E.g. f,foo/bashrc,.bashrc
In the above example, this script will...
1. Create a symbolic link at \"~/.bashrc\"...
2. That points to the file at \"~/foo/bashrc\".

E.g. d,foo/my_config,.config/some_config/my_config
In the above example, this script will...
1. Create a symbolic link at \"~/.config/some_config/my_config\"...
2. That points to the directory at \"~/foo/my_config\".
"
}

print_error()
{
    print_red "$1" ; echo # Error message
    print_red "$2" ; echo # Breaking argument
    if [ "$3" != "" ]
    then
        print_red "$3" ; echo # Additional stuff, if any
    fi
    exit 1
}


# Script: #


case "$1" in
    "$flag_help"|"$flag_help_abbrev")
        print_help
        exit 0
        ;;
    "$flag_create"|"$flag_create_abbrev"|"$flag_delete"|"$flag_delete_abbrev")
        if [ "$#" -lt 2 ]
        then
            print_red "Incorrect number of arguments!" ; echo
            print_red "Please read the help documentation." ; echo
            exit 1
        fi
        if [ ! -r "$2" ]
        then
            print_red "Failed to find or read file \"$2\"!" ; echo
            exit 1
        fi
        config_validate "$2"
        case "$1" in
            "$flag_create"|"$flag_create_abbrev")
                print_yellow "Creating symlinks..." ; echo
                link_create "$2"
                exit 0
                ;;

            "$flag_delete"|"$flag_delete_abbrev")
                print_yellow "Deleting symlinks..." ; echo
                link_delete "$2"
                exit 0
                ;;
            *)
                print_error "Error:    Non-parsable argument detected!" \
                            "Argument: $1"
                ;;
        esac
        ;;
    *)
        print_error "Error:    Non-parsable argument detected!" \
                    "Argument: $1"
esac
