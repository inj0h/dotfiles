# erikoelrojo's .profile
# --------------------------------------------------------------------------------
export PS1="\u @ \h \w\n$ "                         # prompt
printf "Welcome $USER! \xf0\x9f\x98\x83  \n"        # welcome message 




# aliases
# --------------------------------------------------------------------------------
# general
alias emacs='printf "\xf0\x9f\x98\x90  Sorry, this system does not support that software.\n"'
alias ls='ls -aF'                                   
alias vi=/usr/local/Cellar/vim/7.4.1655/bin/vim
alias vim=/usr/local/Cellar/vim/7.4.1655/bin/vim

# ssh 
alias compute='ssh -Y eric.chung@compute.cse.tamu.edu'
alias linux='ssh -Y eric.chung@linux.cse.tamu.edu'

# git
alias git_shove='./.bin/git_shove.sh'
alias git_up_subs='git submodule foreach git pull origin master'
