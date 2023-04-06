
if whence dircolors >/dev/null; then
	eval "$(dircolors -b)"
	zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
	alias ls='ls --color'
else
	export CLICOLOR=1
	zstyle ':completion:*:default' list-colors ''
fi

alias python='python3'
alias pip='pip3'

export PATH="~/.local/bin:$PATH"
export PICO_SDK_PATH=$(realpath ~/Projects/pico-sdk)

