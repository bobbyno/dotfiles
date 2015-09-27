export EDITOR='emacsclient'
export JAVA_HOME=`/usr/libexec/java_home`
export JDK_HOME=`/usr/libexec/java_home`
export LSCOLORS=gxfxcxdxbxegedabagacad
export PATH=/usr/local/sbin:/usr/local/bin:/usr/local/heroku/bin:/Applications/MATLAB_R2014b.app/bin:$PATH
export SHELL=/usr/local/bin/bash

alias be="bundle exec"
alias dev="cd $DEV_HOME"
alias ec="emacsclient -n"
alias gpr="git pull --rebase"
alias gst="git status"
alias htop="sudo htop"
alias ll="ls -alG"
alias ls="ls -G"
alias more="less"
alias mvn-skip="mvn package -Dmaven.test.skip=true"
alias pi="pip install -r requirements.txt"
alias ping="ping -c 10"
alias q="rlwrap ~/q/m32/q"
alias rlf="rlwrap lein figwheel"
alias rlr="rlwrap lein repl"
alias top=htop
alias tree="tree -C"
alias utc="date -u"

PROMPT_DIRTRIM=2

if [ -n "$INSIDE_EMACS" ]; then
  PS1="[\w]\$ "
else
  PS1="[\t][\u:\w]\$ "
fi

# pager
[ `which hilite` ] || echo ".bashrc: You need to install hilite..."
export LESSOPEN="|/usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS="-R -s -F -X"
export PAGER="less -s -F -X"

# python env: virtualenv, virtualenvwrapper, and autoenv
export WORKON_HOME=$DEV_HOME/.virtualenvs
export PROJECT_HOME=$DEV_HOME/dev
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
source /usr/local/bin/virtualenvwrapper.sh
## autoenv
source /usr/local/bin/activate.sh
# end python env

# autocomplete
complete -C aws_completer aws
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

export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
