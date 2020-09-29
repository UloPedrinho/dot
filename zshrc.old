# .zshrc
#
# <2012-08-30 Xov>

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh_history

# include user path
PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/lib/gradle/gradle-6.5.1/bin

### Alias

# debian
if [[ -r /etc/debian_version ]] ; then
    alias acs='apt-cache search'
    alias acsh='apt-cache show'
    alias acp='apt-cache policy'
    alias dlg='dpkg -l | grep'
    alias dL='dpkg -L'
fi

alias ee='emacsclient'

alias ls='ls --color=auto --group-directories-first'
alias grep='grep --color'
alias gtick="aoss gtick"
alias lsl="less /var/log/syslog"
alias df='df -h'
alias home='df -h| grep dev| grep home'
alias banshee='aoss banshee'
alias urxvt-big='urxvt -fn "xft:Monospace:pixelsize=25"'
alias urxvt-medium='urxvt -fn "xft:Bitstream Vera Sans Mono:pixelsize=15"'
alias o=xdg-open

## prompt
autoload colors zsh/terminfo
colors

# test if is on Emacs multi-term
# [INFO-ZSH]6.4 Alternate Forms For Complex Commands
# if [ -n "$INSIDE_EMACS" ] ; then
#     BACKGROUND="white"
#     FOREGROUND="black"
# else
#    BACKGROUND="black"
#    FOREGROUND="white"
# fi


# prompt host colors
# my 'hosts' colors (do you has shutdown a production server?)

if [[ $HOST = Zskani ]]; then
    BGCOLOR="green"
elif [[ $HOST = montgomery ]]; then
	BGCOLOR="blue"
elif [[ $HOST = Zsdrums ]] ; then
    	BGCOLOR="blue"
elif [[ $HOST = Zgintoki ]]; then
	BGCOLOR="red"
fi

FOREGROUND="white"
#PS1="%{${bg[$BGCOLOR]}${fg[$FGCOLOR]}%}%n@%m%{${bg[$BACKGROUND]}${fg[$FOREGROUND]}%}:%~$ "
PS1="%{${fg[$BGCOLOR]}%}%n@%m%{${bg[$BACKGROUND]}${fg[$FOREGROUND]}%}:%~$ "



# export LS_COLORS (from 'dircoloros`)
LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:'
export LS_COLORS


# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2 eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

date


# to see
# -
# man zshcompsys
# `ZLE  Functions'  in  zshcontrib(1)
# 	zshexpn(1).
#  `Shell Grammar' in zshmisc(1).
# `Functions' in zshmisc(1)



### added from zshrc.grml
##
## SEE MORE OPTIONS ::  wget -O .zshrc http://git.grml.org/f/grml-etc-core/etc/zsh/zshrc

export EDITOR=${EDITOR:-emacsclient -c }

function zrcautoload() {
    emulate -L zsh
    setopt extended_glob
    local fdir ffile
    local -i ffound

    ffile=$1
    (( found = 0 ))
    for fdir in ${fpath} ; do
        [[ -e ${fdir}/${ffile} ]] && (( ffound = 1 ))
    done

    (( ffound == 0 )) && return 1
    if [[ $ZSH_VERSION == 3.1.<6-> || $ZSH_VERSION == <4->* ]] ; then
        autoload -U ${ffile} || return 1
    else
        autoload ${ffile} || return 1
    fi
    return 0
}


## Emacs configs

# set EMACS to Emacs shell
[[ $EMACS = t ]] && unsetopt zle

# press esc-e for editing command line in $EDITOR or $VISUAL
if zrcautoload edit-command-line && zle -N edit-command-line ; then
    #k# Edit the current line in \kbd{\$EDITOR}
    bindkey '\ee' edit-command-line
fi


## zstyle

# automatically complete 'cd -<tab>' and 'cd -<ctrl-d>' with menu
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# show menu cursor
zstyle ':completion:*'               menu select=1


# Don't send SIGHUP to background processes when the shell exits.
setopt nohup

# don't complete backup files as executables
#zstyle ':completion:*:complete:-command-::commands' ignored-patterns '(aptitude-*|*\~)'

# command for process lists, the local web server details and host completion
# zstyle ':completion:*:urls' local 'www' '/var/www/' 'public_html'


# remove chars from part of a word
# default: *?_-.[]~=&;!#$%^(){}<>
WORDCHARS=${WORDCHARS//[\/]}

### my variables
TERM=screen-256color


### my functions

dt() { du -h $1|tail -1}

### xterm
# xrdb -l ~/.Xdefaults

### background color
xsetroot -solid black

### silence bell
unsetopt BEEP

### z.sh
. ~/lib/z/z.sh

# try to mount remote directories ; sshfs
# sshfs_Zoh.zsh

# start fecthmail [FIXME]
#
# - TODO :: test if exist
#fetchmail -d 300 &> /dev/null

# emacs
#run-emacs-daemon-andOr-open-client.zsh &> /dev/null

### stumpw
