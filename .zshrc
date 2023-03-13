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

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="agnoster"
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# NOTE: See https://github.com/catppuccin/zsh-syntax-highlighting
# tl;dr if present must be sourced before loading zsh-syntax-highlighting
if [ -e $ZSH/custom/plugins/catppuccin-zsh-syntax-highlighting ]; then
	source $ZSH/custom/plugins/catppuccin-zsh-syntax-highlighting/*.zsh
fi

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
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
prompt_context(){}
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

source /home/dan/.config/broot/launcher/bash/br

autoload -U +X bashcompinit && bashcompinit
# terraform autocomplete
if type "terraform" &> /dev/null; then                                                                                                                                                                            
complete -o nospace -C /home/linuxbrew/.linuxbrew/Cellar/terraform/1.3.9/bin/terraform terraform
fi

export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
