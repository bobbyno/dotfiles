export SVN_EDITOR='mate -w'
export EDITOR='mate -w'
export LESS="-Nmsx4erX"
export PATH=$PATH:$HOME/.rvm/bin:/usr/local/sbin:/usr/local/bin
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export LSCOLORS=gxfxcxdxbxegedabagacad
export JAVA_HOME=`/usr/libexec/java_home`

alias ls="ls -G"
alias ll="ls -alG"
alias more="less"
alias be="bundle exec"
alias tree="tree -C"

PS1="[\t][\u:\w]\$ "

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

