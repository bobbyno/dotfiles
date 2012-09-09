export JAVA_HOME=/Library/Java/Home
export SVN_EDITOR='mate -w'
export LESS="-Nmsx4erX"
# export MANPATH=/opt/local/share/man:$MANPATH
export PERL_CPANM_OPT="--local-lib=~/perl5"
export PERLLIB="/Users/saslani/perl5/lib/perl5"
export PATH=/usr/local/bin:/usr/local/sbin:/usr/local/share/python3:$PATH
# export GRADLE_HOME=/opt/local/share/java/gradle
# export GROOVY_HOME=/opt/local/share/java/groovy
export HOMEBREW_TEMP=/private/tmp
export LSCOLORS=gxfxcxdxbxegedabagacad
export JAVA_HOME=`/usr/libexec/java_home`

alias ls="ls -G"
alias ll="ls -alG"
alias more="less"
alias be="bundle exec"
alias tree="tree -C"
alias bzip="bzip2"
alias mongodb="mongod run --config /usr/local/etc/mongod.conf"
alias mysql="/usr/local/mysql/bin/mysql"
alias mysqladmin="/usr/local/mysql/bin/mysqladmin"
alias gremlin="~/dev/opensource/tinkerpop/gremlin/gremlin-groovy/target/gremlin-groovy-2.1.0-SNAPSHOT-standalone/bin/gremlin-groovy.sh"

PS1="[\t][\w]\$ "

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

