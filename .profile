# init 
# --------------------------------------------------------------------------------
export PS1="\u @ \h \w\n$ "                         # prompt
printf "Welcome $USER! \xf0\x9f\x98\x84  \n"        # welcome message 

# set non-login cnfg to .bashrc (macOS doesn't default to this)
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
