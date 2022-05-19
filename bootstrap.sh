# if ubuntu 22.04, remvoe needrestart because it spams and arguably shouldn't be installed
# sudo apt-get purge needrestart

# set up dotfiles repo

ln -s ~/dotfiles/.zshrc ~/
ln -s ~/dotfiles/.p10k.zsh ~/
ln -s ~/dotfiles/.aliases ~/

# zsh

apt install zsh
sudo chsh -s /usr/bin/zsh

# oh-my-zsh
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

# nvim config for working w/ vscode
ln -s ~/dotfiles/.config ~/

# install nvim 0.7
wget https://github.com/neovim/neovim/releases/download/v0.7.0/nvim-linux64.deb
sudo apt install ./nvim-linux64.deb

# global git config
ln -s ~/dotfiles/.gitignore ~/
ln -s ~/dotfiles/.gitconfig ~/
git config --global core.excludesFile '~/.gitignore'
git config --global core.editor "nvim"

# install homebrew
y | /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

test -d ~/.linuxbrew && eval "$(~/.linuxbrew/bin/brew shellenv)"
test -d /home/linuxbrew/.linuxbrew && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
test -r ~/.bash_profile && echo "eval \"\$($(brew --prefix)/bin/brew shellenv)\"" >> ~/.bash_profile
echo "eval \"\$($(brew --prefix)/bin/brew shellenv)\"" >> ~/.profile

# broot - better tree(1)
brew install broot

# fzf - fuzzy completion
brew install fzf
# To install useful key bindings and fuzzy completion:
$(brew --prefix)/opt/fzf/install
source $HOME/.zshrc

# better manpages
brew install tldr

wget https://github.com/Peltoche/lsd/releases/download/0.21.0/lsd_0.21.0_amd64.deb
sudo dpkg -i lsd_0.21.0_amd64.deb
rm ~/lsd_0.21.0_amd64.deb

# optional: install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# TODO optional: install golang

# TODO: doesn't seem to add fd binary
#sudo apt install fd-find
brew install fd
sudo apt install ripgrep
sudo apt install sqlite

sudo apt install gcc

# install npm
brew install pnpm

sudo add-apt-repository ppa:kelleyk/emacs
# native comp emacs28
sudo apt install emacs28

# doom!
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install

rm -r ~/.doom.d
ln -s ~/dotfiles/.doom.d ~/

doom sync

# install lunarvim

# cpp build tools needed to compile treesitter
sudo apt install build-essential

brew install codespell

# NOTE: on wsl, you need to install win32yank to enable shared system clipboard
# `choco install win32yank`

# interactive installer
y | bash <(curl -s https://raw.githubusercontent.com/lunarvim/lunarvim/master/utils/installer/install.sh)

ln -sf ~/dotfiles/.config/lvim/config.lua ~/.config/lvim/config.lua
