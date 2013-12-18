export EDITOR='emacsclient'
export LESS="-Nmsx4erX"
export PATH=$HOME/.rvm/bin:/usr/local/sbin:/usr/local/bin:/usr/local/heroku/bin:$PATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export LSCOLORS=gxfxcxdxbxegedabagacad
export JAVA_HOME=`/usr/libexec/java_home`

alias ls="ls -G"
alias ll="ls -alG"
alias more="less"
alias tree="tree -C"
alias be="bundle exec"
alias gremlin="~/dev/gremlin/gremlin-groovy-1.5/gremlin-groovy.sh"
alias mvn-skip="mvn package -Dmaven.test.skip=true"
alias pgstart="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
alias pgstop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias ec="emacsclient -n"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs &"
alias mongostart="mongod run --config /usr/local/etc/mongod.conf"

PS1="[\t][\u:\w]\$ "

. ~/.bash_private

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
