#!/bin/bash

DOTFILES_DIR=$PWD

installed() {
	val=$(type "$1" >/dev/null 2>&1)
	return $val
}

# before we forget
sudo apt update

# if ubuntu 22.04, remove needrestart because it spams and seems harmless to remove
# sudo apt-get purge needrestart

echo "Linking dotfiles"
ln -sf $DOTFILES_DIR/.zshrc $HOME/
ln -sf $DOTFILES_DIR/.p10k.zsh $HOME/
ln -sf $DOTFILES_DIR/.aliases.sh $HOME/

if ! installed "zsh"; then
	echo "zsh is not installed. Installing zsh..."
	sudo apt install zsh
	sudo chsh -s /usr/bin/zsh
	if ! installed "zsh"; then
		echo "zsh installation failed and is required for these dotfiles\n"
		exit 1
	fi
	echo "zsh is now installed\n"
fi

# TODO: migrate away from oh-my-zsh
echo "Installing oh-my-zsh"
if ! echo $ZSH_CUSTOM | grep "oh-my-zsh"; then
	sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/catppuccin/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting-catppuccin

if ! [[ -d "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting-catppuccin/themes/selected_theme" ]]; then
	mkdir -p ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting-catppuccin/themes/selected_theme
	cp ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting-catppuccin/themes/selected_theme/catppuccin.zsh-theme ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting-catppuccin/themes/catppuccin_macchiato-zsh-syntax-highlighting.zsh ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting-catppuccin/themes/selected_theme/
fi

ln -fs /.config ~/.config

# install nvim 0.8.2 or latest
if ! installed "nvim"; then
	wget https://github.com/neovim/neovim/releases/download/v0.8.2/nvim-linux64.deb
	sudo apt install ./nvim-linux64.deb
	rm ./nvim-linux64.deb
fi

# global git config
ln -s $DOTFILES_DIR/.gitignore $HOME/
ln -s $DOTFILES_DIR/.gitconfig $HOME/
git config --global core.excludesFile "$HOME/.gitignore"
git config --global core.editor "nvim"

# install homebrew
y "yes" | /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

test -d ~/.linuxbrew && eval "$(~/.linuxbrew/bin/brew shellenv)"
test -d /home/linuxbrew/.linuxbrew && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
test -r ~/.bash_profile && echo "eval \"\$($(brew --prefix)/bin/brew shellenv)\"" >>~/.bash_profile
echo "eval \"\$($(brew --prefix)/bin/brew shellenv)\"" >>~/.profile

echo "\n***** Installing languages *****\n"
# debian-based distros come with old node; should install new version
echo "  Installing node"
brew install node

if ! installed "lua"; then
	echo "  Installing lua"
	brew install lua
fi

if ! installed "python"; then
	echo "  Installing python 3.11"
	brew install python@3.11
	if ! installed "python" && installed "python3"; then
		# if it's stupid and it works, it's not stupid ;)
		cp /home/linuxbrew/.linuxbrew/bin/python3 /home/linuxbrew/.linuxbrew/python
	fi
fi
# install rust and cargo
if ! installed "cargo"; then
	echo "\tInstalling rust and cargo"
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	source $HOME/.cargo/env
fi
if ! installed "go"; then
	# TODO: install golang @ latest
	echo "\tInstalling golang"
	curl -OL https://golang.org/dl/go1.20.2.linux-amd64.tar.gz
	rm -rf /usr/local/go && tar -C /usr/local -xzf go1.20.2.linux-amd64.tar.gz
fi

echo "\n***** Installing misc dependencies and utilities *****\n"
if ! installed "broot"; then
	brew install broot
fi

if ! installed "lazygit"; then
	brew tap jesseduffield/lazygit
	brew install lazygitbrew
fi

if ! installed "fzf"; then
	# fzf - fuzzy find all the things
	sudo apt install fzf
	# install useful shell key bindings and fuzzy completion:
	$(brew --prefix)/opt/fzf/install
fi

brew install pgcli mycli
# better manpages
brew install tldr

brew install syncthing

# better ls
if ! installed "lsd"; then
	wget https://github.com/Peltoche/lsd/releases/download/0.21.0/lsd_0.21.0_amd64.deb
	sudo dpkg -i lsd_0.21.0_amd64.deb
	rm ~/lsd_0.21.0_amd64.deb
fi

sudo apt install ripgrep
sudo apt install sqlite
brew install fd
brew install jansson
brew install cmake
brew install libtool
# needed to compile vterm, among other things
brew install gcc@5
# cpp build tools needed to compile treesitter
brew install build-essential

brew install pnpm
brew install node
brew install codespell

cargo install stylua

go install mvdan.cc/sh/v3/cmd/shfmt@latest
sudo apt install shellcheck

# best top
npm install -g vtop
# neovim the package for npm (for LSP); not the editor
npm install -g neovim

# ranger file manager
pip install ranger-fm

if ! installed "emacs"; then
	echo "\n***** Installing emacs and dependencies *****\n"
	sudo add-apt-repository ppa:kelleyk/emacs
	# native comp emacs28. Disable if using slow CPU
	sudo apt install emacs28-nativecomp
fi

# DOOM!
if ! installed "doom"; then
	echo "\n***** Installing doom emacs distribution *****\n"
	git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
	~/.emacs.d/bin/doom install

	rm -r ~/.doom.d
	ln -sf $DOTFILES_DIR/.doom.d ~/

	echo '\tRun "$HOME/.emacs.d/bin/doom sync" to process doom emacs installation and looooong package compilation'
fi

# NOTE: on wsl, you need to install win32yank to enable shared system clipboard via `choco install win32yank`

if ! installed "lvim"; then
	echo "\n***** Installing lunarvim *****\n"
	y yes | bash <(curl -s https://raw.githubusercontent.com/lunarvim/lunarvim/master/utils/installer/install.sh)
fi

# TODO: update to use the new config (migrate deps)
# ln -sf ~/dotfiles/.config/lvim/config.lua ~/.config/lvim/config.lua

if ! [[ -d $DOTFILES_DIR/../lvim ]]; then
	git clone git@github.com:varyag00/lvim.git $DOTFILES_DIR/../lvim
fi
echo "\tLinking lvim config files..."

ln -sf $DOTFILES_DIR/./lvim/* $HOME/.config/lvim/
if echo $?; then
	echo "\tSUCCESS!"
else
	echo "\tERROR!"
	exit 1
fi

echo "\n***** Not installing optional packages; check them out and run manually"
if 1; then
	echo "\tInstalling neovim"
	sudo apt install protonvpn
	# there's a tray icon too: https://protonvpn.com/support/linux-ubuntu-vpn-setup/
fi

echo "\n***** Installing docker, k8s, and infra tooling *****\n"
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl

echo "\tInstalling docker engine.."
sudo apt update
sudo apt install \
	ca-certificates \
	curl \
	gnupg \
	lsb-release

# repo requires gpg keys
sudo mkdir -m 0755 -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
echo \
	"deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list >/dev/null
sudo apt update
sudo apt install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

if ! installed "docker"; then
	echo "\tFailed to install docker engine. Manually install it..."
	echo "\t\thttps://docs.docker.com/engine/install/ubuntu/"
	echo "\t\thttps://docs.docker.com/engine/install/macos/"
else
	echo "\tSuccesfully installed docker. Run linux post install: https://docs.docker.com/engine/install/linux-postinstall/"
	echo "\t\tsudo groupadd docker"
	echo "\t\tsudo usermod -aG docker \$USER"
	echo "\t\tnewgrp docker"
	echo "\t\tsystemctl enable docker.service"
	echo "\t\tsystemctl enable containerd.service"
fi

# kubectx = contectd switching; also installs kubens = cluster switching
brew install kubectx

echo "\tInstalling k6 load testing tool"
sudo gpg --no-default-keyring --keyring /usr/share/keyrings/k6-archive-keyring.gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys C5AD17C747E3415A3642D57D77C6C491D6AC1D69
echo "deb [signed-by=/usr/share/keyrings/k6-archive-keyring.gpg] https://dl.k6.io/deb stable main" | sudo tee /etc/apt/sources.list.d/k6.list
sudo apt-get update
sudo apt-get install k6

echo "\tInstalling ansible"
python -m pip install --user ansible

echo "\tInstalling terraform"
brew tap hashicorp/tap
brew install hashicorp/tap/terraform
