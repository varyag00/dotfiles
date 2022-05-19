alias manage="python manage.py"

if type "lvim" &> /dev/null; then                                                                                                                                                                            
    alias vim=lvim
    alias vi=lvim
    alias nvim=lvim
elif type "nvim" &> /dev/null; then
    alias vim=nvim
    alias vi=nvim
fi

if type "lsd" &> /dev/null; then                                                                                                                                                                            
    alias ls="lsd"
    alias la="lsd --long --all --group"
fi
