export EDITOR='subl -w'
export LESS="-Nmsx4erX"
export PATH=$HOME/.rvm/bin:/usr/local/sbin:/usr/local/bin:$PATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export LSCOLORS=gxfxcxdxbxegedabagacad
export JAVA_HOME=`/usr/libexec/java_home`

alias ls="ls -G"
alias ll="ls -alG"
alias more="less"
alias tree="tree -C"
alias be="bundle exec"
alias gremlin="~/dev/gremlin/gremlin-groovy-1.5/gremlin-groovy.sh"
alias fspec="foreman run rake spec"
alias mvn-skip="mvn package -Dmaven.test.skip=true"
alias pgstop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias ci="ssh teamcity@ec2-54-234-143-238.compute-1.amazonaws.com"
alias frlm="foreman run lein midje"
alias mpfs="mvn package && foreman start"
alias fs="foreman start"
alias ec="emacsclient"

PS1="[\t][\u:\w]\$ "

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

