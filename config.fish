# Filename: config.fish
# Note:     Friendly settings for friendly shell.

#
# Abbreviations
#

# Git
abbr -a ga  'git add'
abbr -a gb  'git branch'
abbr -a gc  'git commit'
abbr -a gch 'git checkout'
abbr -a gd  'git diff'
abbr -a gl  'git log'
abbr -a gs  'git status'

# Utility
abbr -a em  'emacsclient -n'
abbr -a ff  'find . -type f -iname'
abbr -a ffd 'find . -type d -iname'
abbr -a ffi 'find . -iname'
abbr -a o   'ls -AGho'
abbr -a t   'tree -aC .'
abbr -a u   'cd ..'

#
# Aliases
#

# Convenience
if [ (uname -s) = "Darwin" ]
  alias go   'pwd | pbcopy'
  alias here 'cd (pbpaste)'
end

# Git
alias groot  'cd (git rev-parse --show-toplevel)'
alias gsroot 'cd (git rev-parse --show-superproject-working-tree)'
alias ls     ls # unset default flags

#
# Environment Variables
#

set -Ux EDITOR "emacsclient -nw"

#
# Greeting
#

function fish_greeting
  set_color red
  echo      "$nge_boot_kanji "
  set_color white
  echo -n   "\"Entering Terminal Dogma.\""
  echo -n   " |$quotel_ja ターミナルドグマに入る。$quoter_ja"
  echo      "| \"터미널 도그마에 들어간다.\""
  load_config_local
end

#
# Unicode
#

set greek_delta        (printf '\u0394')
set greek_lambda       (printf '\u03bb')
set math_equal_not     (printf '\u2260')
set math_exists        (printf '\u2203')
set nge_boot_kanji     (printf '\u8d77\u52d5')
set quotel_ja          (printf '\u300E')
set quoter_ja          (printf '\u300F')
set vcs_icon_arrow_ltr (printf '\u21fe')
set vcs_icon_check     (printf '\u2714')
set vcs_icon_cross     (printf '\u2718')

#
# Local Configuration
#

function load_config_local
  set -l config_local "$HOME/.config/fish/local.fish"
  echo "Loading local configuration..."
  if [ -r $config_local ]
    echo "Found."
    source $config_local
    echo "Loaded."
  else
    echo "Unable to detect local configuration."
  end
end

#
# Path
#

switch (uname -s)
  case "Darwin"
    # Add these directories to the PATH for Homebrew.
    set -g fish_user_paths "/Library/TeX/texbin" $fish_user_paths
    set -g fish_user_paths "/usr/local/bin"      $fish_user_paths
    set -g fish_user_paths "/usr/local/sbin"     $fish_user_paths
    # case "Linux"
    # case '*'
end

#
# Prompt
#

function fish_prompt
  set_color blue
  if [ $PWD = $HOME ]
    printf '~'
  else
    printf '%s' (basename $PWD)
  end
  set_color white
  printf '%s' (__fish_git_prompt)
  set_color green
  echo " $greek_lambda "
end

#
# Version Control
#

# Git
set __fish_git_prompt_shorten_branch_len      20
set __fish_git_prompt_show_informative_status
set __fish_git_prompt_showcolorhints
set __fish_git_prompt_showdirtystate
set __fish_git_prompt_showstashstate
set __fish_git_prompt_showuntrackedfiles
set __fish_git_prompt_showupstream            "none"

set __fish_git_prompt_char_cleanstate         " $vcs_icon_check"
set __fish_git_prompt_char_dirtystate         " $vcs_icon_cross"
set __fish_git_prompt_char_invalidstate       " $math_equal_not"
set __fish_git_prompt_char_stagedstate        " $vcs_icon_check"
set __fish_git_prompt_char_stashstate         " $math_exists"
set __fish_git_prompt_char_stateseparator     " $vcs_icon_arrow_ltr"
set __fish_git_prompt_char_untrackedfiles     " $greek_delta"

set __fish_git_prompt_color_branch            magenta --bold
set __fish_git_prompt_color_branch_detached   red
set __fish_git_prompt_color_cleanstate        green
set __fish_git_prompt_color_dirtystate        red
set __fish_git_prompt_color_invalidstate      red
set __fish_git_prompt_color_stagedstate       yellow
set __fish_git_prompt_color_stashstate        green
set __fish_git_prompt_color_untrackedfiles    red
