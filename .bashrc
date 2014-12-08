export SHELL=/usr/local/bin/bash
export EDITOR='emacsclient'
export LESS="-Nmsx4erX"
export PATH=/usr/local/sbin:/usr/local/bin:/usr/local/heroku/bin:$PATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export LSCOLORS=gxfxcxdxbxegedabagacad
export JAVA_HOME=`/usr/libexec/java_home`
export JDK_HOME=`/usr/libexec/java_home`
export RBENV_ROOT=/usr/local/var/rbenv
export DOCKER_HOST=tcp://local-docker.outpace.com:2375

alias ls="ls -G"
alias ll="ls -alG"
alias more="less"
alias tree="tree -C"
alias be="bundle exec"
alias gremlin="~/dev/gremlin/gremlin-groovy-1.5/gremlin-groovy.sh"
alias mvn-skip="mvn package -Dmaven.test.skip=true"
alias pgstart="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
alias pgstop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias mongostart="mongod run --config /usr/local/etc/mongod.conf"
alias dev="cd ~/dev"
alias practice="cd ~/dev/practice/clojure"
alias es="elasticsearch --config=/usr/local/opt/elasticsearch/config/elasticsearch.yml"
alias pi="pip install -r requirements.txt"
alias emacs="emacs &"
alias vgs="vagrant global-status"

PROMPT_DIRTRIM=2

. ~/.outpace_profile

if [ -n "$INSIDE_EMACS" ]; then
  PS1="[\w]\$ "
else
  PS1="[\t][\u:\w]\$ "
fi

# autocomplete
complete -C aws_completer aws
. /usr/local/etc/bash_completion.d/git-completion.bash
. ~/complete_make_targets.sh

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"
source "$HOME/.outpace/bashrc"
