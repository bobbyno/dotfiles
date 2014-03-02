export SHELL=/usr/local/bin/bash
export EDITOR='emacsclient'
export LESS="-Nmsx4erX"
export PATH=/usr/local/sbin:/usr/local/bin:/usr/local/heroku/bin:/usr/local/share/python:$PATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export LSCOLORS=gxfxcxdxbxegedabagacad
export JAVA_HOME=`/usr/libexec/java_home`
export JDK_HOME=`/usr/libexec/java_home`
export RBENV_ROOT=/usr/local/var/rbenv

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
alias dev="cd ~/dev"
alias practice="cd ~/dev/practice/clojure"
alias es="elasticsearch --config=/usr/local/opt/elasticsearch/config/elasticsearch.yml"

PROMPT_DIRTRIM=2

. ~/.outpace_profile

if [ -n "$INSIDE_EMACS" ]; then
  PS1="[\w]\$ "
else
  PS1="[\t][\u:\w]\$ "
fi

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
