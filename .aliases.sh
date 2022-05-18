alias manage="python manage.py"

if [ command -v nvim &> /dev/null ]; then
    alias vim=nvim
fi

if [ command -v lsd &> /dev/null ]; then
    alias ls="lsd"
    alias la="lsd --long --all --group"
fi
