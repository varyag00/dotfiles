alias manage="python manage.py"

if type "lvim" &>/dev/null; then
	alias vim=lvim
	alias vi=lvim
	alias nvim=lvim
elif type "nvim" &>/dev/null; then
	alias vim=nvim
	alias vi=nvim
fi

if type "lsd" &>/dev/null; then
	alias ls="lsd"
	alias la="lsd --long --all"
fi

if type "vtop" &>/dev/null; then
	alias top="vtop"
fi

if type "terraform" &>/dev/null; then
	alias tf="terraform"
fi
