# Add user-specific settings that aren't version controlled:
## set DEV_HOME in that file
private_settings="$HOME/.`whoami`.bashrc"
if [ -e $private_settings ]; then
    . $private_settings
fi

# env vars
export EDITOR='emacsclient'
export JAVA_HOME=`/usr/libexec/java_home`
export JDK_HOME=`/usr/libexec/java_home`
export LSCOLORS=gxfxcxdxbxegedabagacad
export PATH=/usr/local/sbin:/usr/local/bin:/usr/local/heroku/bin:/Applications/MATLAB_R2014b.app/bin:$(npm bin):$PATH
export SHELL=/usr/local/bin/bash

# aliases
alias be="bundle exec"
alias brewup="brew update && brew cleanup; brew doctor"
alias dev="cd $DEV_HOME"
alias diff="colordiff"
alias ec="emacsclient -n"
alias emacs="emacs -nw"
alias http="python -m http.server"
alias htop="sudo htop"
alias ll="ls -alG"
alias ls="ls -G"
alias marked="/Applications/Marked\ 2.app/Contents/MacOS/Marked\ 2"
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

# navigation aliases
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'

# pager
[ `which hilite` ] || echo ".bashrc: You need to install hilite..."
export LESSOPEN="|/usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS="-R -s -F -X"
export PAGER="less -s -F -X"

# autocomplete
for f in /usr/local/etc/bash_completion.d/*; do
  . $f
done
complete -C aws_completer aws
. ~/make_target_completion.bash

# prompt
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1

PROMPT_DIRTRIM=2
if [ -n "$INSIDE_EMACS" ]; then
  PS1='[\w]\$ '
else
  PS1='[\t][\u:\w] \[\033[01;32m\]$(__git_ps1 "(%s) ")\[\033[00m\]\$ '
fi

# python env: virtualenv, virtualenvwrapper, and autoenv
export WORKON_HOME=$DEV_HOME/.virtualenvs
export PROJECT_HOME=$DEV_HOME/dev
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
source /usr/local/bin/virtualenvwrapper.sh
## autoenv
source /usr/local/bin/activate.sh

# ruby
export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
