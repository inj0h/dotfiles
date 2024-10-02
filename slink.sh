#!/bin/sh

# A POSIX compliant shell script to setup and teardown symbolic links for a set
# of files outlined by an argument configuration file.


### Variables:


# Arguments
flag_help="--help"
flag_help_abbrev="-h"
flag_create="--create"
flag_create_abbrev="-c"
flag_delete="--delete"
flag_delete_abbrev="-d"

# Configuration file
configuration_file="dotfiles.txt"

# Link Actions
link_action_link="link"
link_action_unlink="unlink"

# Printing
color_red="\033[0;31m"
color_green="\033[0;32m"
delimiter="--------------------------------------------------"


### Functions:


link_create()
{
    # This function will parse the $configuration_file, and create a
    # symbolic link for each file per line at the specified directory.
    #
    # It short circuits the script upon encountering an error, and
    # prints the cause of error to STDOUT.
    #
    # It assumes that the $configuration_file exists and its contents,
    # parsable.

    # Detect whether the host has link, ln, installed and set the
    # respective flags for creating symbolic links.
    if ! command -v ln > /dev/null
    then
        echo "Error: Unable to find link binary on system."
        echo "Exiting with nonzero code.."
        exit 1
    fi

    case "$(uname -s)" in
        "Darwin")
            echo "BSD/Mac system detected."
            echo "Setting flags.."
            ln_flags="-shfv"
            ;;
        "Linux")
            echo "Linux system detected."
            echo "Setting flags.."
            ln_flags="-sTfv"
            ;;
        *)
            echo "Error: Incompatible system detected."
            echo "Exiting with nonzero code.."
            exit 1
    esac

    echo "Done."
    echo "Linking.."
    echo "$delimiter"

    while IFS="," read -r filetype filepath linkpath
    do
        filepath_full="$HOME/$filepath"
        linkpath_full="$HOME/$linkpath"

        ln "$ln_flags" "$filepath_full" "$linkpath_full"
    done < "$1"

    echo "$delimiter"
    echo "Done."
}


link_delete()
{
    # This function will parse the $configuration_file, and will remove
    # the symbolic link for each file, i.e. the rightmost comma
    # separated value.
    #
    # It short circuits the script upon encountering an error, and
    # prints the cause of error to STDOUT.
    #
    # It assumes that the $configuration_file exists and its contents,
    # parsable.

    echo "Unlinking.."
    echo "$delimiter"
    echo "Removing.."

    while IFS="," read -r filetype filepath linkpath
    do
        linkpath_full="$HOME/$linkpath"
        linkpath_full_noexpand="${HOME:?}/$linkpath"

        rm -rf "$linkpath_full_noexpand"
        echo "-> $linkpath_full"
    done < "$1"

    echo "$delimiter"
    echo "Done."
}


print_color() { printf "$1%s\033[0m" "$2" ; }


print_color_n() { printf "$1%s\033[0m" "$2" ; echo ; }


print_help()
{
    # TODO() Edit this prose so it's not so ridiculously long.

    # This function prints the help documentation for properly running
    # this script.

    echo "Usage:   connect-the-dots [options] [file]

    Options: $flag_help_abbrev, $flag_help
    Print help, i.e. this output.

    $flag_create_abbrev, $flag_create [file]
    Parse the contents of the configuration file, and if it
    contains no errors, i.e. the relevant directories and files
    exist on disk, create symbolic links for the file(s) listed
    per line.

    $flag_delete_abbrev, $flag_delete [file]
    Parse the contents of the configuration file, and if it
    contains no errors, i.e. the relevant directories and files
    exist on disk, delete the symbolic links for the file(s)
    listed per line.

    File:    A plain text configuration file named $configuration_file. It can
    exist anywhere within the user home directory insofar as the
    host filesystem has access. It must have the format below:

    filetype + , + path_to_file + , + path_to_symbolic_link

    The filetype specifies either a directory or file. Any string
    of characters beginning with d specifies that the path points
    to a directory and with f, that the path points to a file.

    Paths must be fully written so their location from the user
    home directory is clear.

    E.g. file,foo/.bashrc,.bashrc
    Conveys the file ~/foo/.bashrc and the respective symbolic link
    at ~/.bashrc.

    E.g. f,foo/.bash_profile,.bash_profile
    Conveys the file ~/foo/.bash_profile and the respective
    symbolic link at ~/.bash_profile.

    E.g. dir,foo/.config.d,.config.d
    Conveys the directory ~/foo/.config.d and the respective
    symbolic link at ~/.config.d.
    "
}


validate_config()
{
    # This function parses the $configuration_file, and validates
    # whether the files marked for linking and the directories where
    # their respective links will get created exist on disk.
    #
    # It short circuits the script upon encountering an error, and
    # prints the cause of error to STDOUT.
    #
    # It assumes that the $configuration_file exists.

    while IFS="," read -r filetype filepath linkpath
    do
        filepath_full="$HOME/$filepath"
        linkpath_directory="$HOME/$(dirname "$linkpath")"

        # TODO(now) Remove print debug statements
        print_color_n "$color_red" "DEBUG:"
        echo "filetype           = $filetype"
        echo "filepath           = $filepath"
        echo "filepath_full      = $filepath_full"
        echo "linkpath           = $linkpath"
        echo "linkpath_directory = $linkpath_directory"

        case "$filetype" in
            "d"*)
                if [ ! -d "$filepath_full" ]
                    then
                    echo "Error:     Could not find directory on disk."
                    echo "Directory: $filepath_full"
                    echo "Exiting with nonzero code.."
                    exit 1
                fi
                ;;

            "f"*)
                if [ ! -f "$filepath_full" ]
                then
                    echo "Error: Could not find file on disk."
                    echo "File:  $filepath_full"
                    echo "Exiting with nonzero code.."
                    exit 1
                fi
                ;;

            "#"*)
                print_color_n "$color_red" "Skipping line!"
                # Treat lines starting with "#" as comments and do not evaluate them.
                ;;

            # TODO() Add support for skipping empty lines.

            *)
                echo "Error:   Non-parsable content"
                echo "Content: $filetype"
                echo "Please make sure $configuration_file has correct formatting."
                echo "Exiting with nonzero code.."
                exit 1
        esac

        # TODO(now) Determine why we needed this.
        if [ ! -d "$linkpath_directory" ]
        then
            echo "Error:     Could not find directory on disk."
            echo "Directory: $linkpath_directory"
            echo "Exiting with nonzero code.."
            exit 1
        fi

    done < "$1"
}


### Script:


case "$1" in
    "$flag_help"|"$flag_help_abbrev")
        print_help
        exit 0
        ;;

    "$flag_create"|"$flag_create_abbrev"|"$flag_delete"|"$flag_delete_abbrev")
        if [ "$#" -lt 2 ]
        then
            # TODO(now) Make the error output in color!
            # TODO(now) Provide more context in print statement
            echo "Incorrect number of arguments!"
            exit 1
        fi

        if [ ! -f "$2" ]
        then
            # TODO(now) Make the error output in color!
            # TODO(now) Provide more context in print statement?
            echo "File $2 does not exist!"
            exit 1
        fi

        validate_config "$2"

        case "$1" in
            "$flag_create"|"$flag_create_abbrev")
                echo "Creating soft links..."
                link_create "$2"
                exit 0
                ;;

            "$flag_delete"|"$flag_delete_abbrev")
                echo "Deleting soft links..."
                exit 0
                link_delete "$2"
                ;;

            *)
                # TODO(now) Make the error output in color!
                # TODO(now) Refactor this into a function
                echo "Error:    Non-parsable argument detected!"
                echo "Argument: $1"
                echo "Exiting with nonzero code.."
                exit 1
                ;;
        esac
        ;;

    *)
        # TODO(now) Make the error output in color!
        # TODO(now) Refactor this into a function
        echo "Error:    Non-parsable argument detected!"
        echo "Argument: $1"
        echo "Exiting with nonzero code.."
        exit 1
esac
