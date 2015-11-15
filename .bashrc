export SHELL=/usr/local/bin/bash
export EDITOR='emacsclient'
export LESS="-Nmsx4erX"
export PATH=/usr/local/sbin:/usr/local/bin:/usr/local/heroku/bin:$PATH
export LSCOLORS=gxfxcxdxbxegedabagacad
export JAVA_HOME=`/usr/libexec/java_home`
export JDK_HOME=`/usr/libexec/java_home`
export RBENV_ROOT=/usr/local/var/rbenv

alias ls="ls -G"
alias ll="ls -alG"
alias more="less"
alias tree="tree -C"
alias ping="ping -c 10"
alias be="bundle exec"
alias mvn-skip="mvn package -Dmaven.test.skip=true"
alias gremlin="~/dev/gremlin/latest/bin/gremlin.sh"
alias pgstart="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
alias pgstop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias mongostart="mongod run --config /usr/local/etc/mongod.conf"
alias dev="cd ~/dev"
alias pi="pip install -r requirements.txt"
alias vgs="vagrant global-status --prune"
alias utc="date -u"
alias gpr="git pull --rebase"
alias gst="git status"
alias emacs="emacs -nw"
alias ec="emacsclient -n"
alias htop="sudo htop"
alias top=htop

PROMPT_DIRTRIM=2

if [ -n "$INSIDE_EMACS" ]; then
  PS1="[\w]\$ "
else
  PS1="[\t][\u:\w]\$ "
fi

# python env: virtualenv, virtualenvwrapper, and autoenv
export WORKON_HOME=$HOME/dev/.virtualenvs
export PROJECT_HOME=$HOME/dev
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
source /usr/local/bin/virtualenvwrapper.sh
## autoenv
source /usr/local/bin/activate.sh
# end python env

# autocomplete
complete -C aws_completer aws
. /usr/local/bin/eb_completion.bash
. /usr/local/etc/bash_completion.d/git-completion.bash
. /usr/local/etc/bash_completion.d/brew_bash_completion.sh
. /usr/local/etc/bash_completion.d/lein-completion.bash
. /usr/local/etc/bash_completion.d/tmux
. /usr/local/etc/bash_completion.d/docker
. ~/make_target_completion.bash

# user-specific settings that aren't version controlled.
private_settings="$HOME/.`whoami`.bashrc"
if [ -e $private_settings ]; then
    . $private_settings
fi

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

