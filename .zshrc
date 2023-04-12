#!/bin/sh

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
# doom emacs
export PATH=$HOME/.emacs.d/bin:$PATH

# Go Global variables
export GOROOT=/usr/local/go
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/dan/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

# NOTE: See https://github.com/catppuccin/zsh-syntax-highlighting
# tl;dr if present, it must be sourced before loading zsh-syntax-highlighting
# add a new `themes/selected_theme` dir and copy selected theme
if [ -e $ZSH/custom/plugins/catppuccin-zsh-syntax-highlighting ]; then
	source $ZSH/custom/plugins/catppuccin-zsh-syntax-highlighting/themes/selected_theme/*.zsh
fi

plugins=(
git
vi-mode
command-not-found
sudo
zsh-syntax-highlighting
zsh-autosuggestions
kubectx
kubectl
)

if [ ! -z $INSIDE_EMACS ]; then
	# only run in emacs shells
fi

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
export KEYTIMEOUT=1

if [ -e $ZSH/oh-my-zsh.sh ]; then
	source $ZSH/oh-my-zsh.sh
fi


# remove user@hostname prompt
# prompt_context(){}
#
# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='lvim'
else
  export EDITOR='lvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# aliases
if [ -e $HOME/.aliases.sh ]; then
	source $HOME/.aliases.sh
fi

if [ -e $HOME/.profile ]; then
	source $HOME/.profile
fi

if [ -e $HOME/.ssh_hosts ]; then
	source $HOME/.ssh_hosts
fi

# extra work-specific setup; NOT VERSION CONTROLLED
if [ -e $HOME/.msv-wks.sh ]; then
	source $HOME/.msv-wks.sh

	# run keychain on shell startup
	eval $(keychain --eval id_rsa)
fi

if [ ! command -v fzf &> /dev/null ]; then
	echo "fzf is not installed, see bootstrap.sh"
	exit
else
	# fzf
	[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
fi

# WSL2 XServer

#export DISPLAY="`grep nameserver /etc/resolv.conf | sed 's/nameserver //'`:0"
#export DISPLAY=192.168.1.201:0

# last working DISPLAY
#export DISPLAY=$(ip route | awk '{print $3; exit}'):0

# from https://www.youtube.com/watch?v=YxQMDBnrMws
#export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0

# 4/26/2021 - testing removing this to see if UI scaling is improved emacs
#export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
export LIBGL_ALWAYS_INDIRECT=1
#export GDK_SCALE=2

# some other options
# export GDK_SCALE=0.5
# export GDK_DPI_SCALE=2

# fix wsl2 interop, allowing emacs to launch windows programs
fix_wsl2_interop() {
	for i in $(pstree -np -s $$ | grep -o -E '[0-9]+'); do
		if [[ -e "/run/WSL/${i}_interop" ]]; then
			export WSL_INTEROP=/run/WSL/${i}_interop
		fi
	done
}

~/.emacs.d/bin/doom env > /dev/null 2>&1

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true
POWERLEVEL9K_INSTANT_PROMPT=quiet

export PNPM_HOME="/home/dan/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"

export PATH="$HOME/.poetry/bin:$PATH"

export SRC="/usr/local/src"

source /home/dan/.config/broot/launcher/bash/br

autoload -U +X bashcompinit && bashcompinit
# terraform autocomplete
if type "terraform" &> /dev/null; then                                                                                                                                                                            
complete -o nospace -C /home/linuxbrew/.linuxbrew/Cellar/terraform/1.3.9/bin/terraform terraform
fi

export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

if type "fuck" &> /dev/null; then                                                                                                                                                                            
  eval $(thefuck --alias)
  eval "$(zoxide init zsh)"
fi
