# [2026-09-24] fnm 初始化已移至本文件末尾，确保 Node 版本优先级不被 ~/.local/bin 遮蔽
# eval "$(fnm env --use-on-cd)"
# export PATH="$HOME/.fnm:$PATH"

# Forcefully remove invalid Homebrew completion path
fpath=(${fpath[@]:#*/usr/local/share/zsh/site-functions})

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="robbyrussell"

# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
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

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='nvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch $(uname -m)"

# Set personal aliases, overriding those provided by Oh My Zsh libs,
# plugins, and themes. Aliases can be placed here, though Oh My Zsh
# users are encouraged to define aliases within a top-level file in
# the $ZSH_CUSTOM folder, with .zsh extension. Examples:
# - $ZSH_CUSTOM/aliases.zsh
# - $ZSH_CUSTOM/macos.zsh
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# 启动代理转发
alias proxy-start='pkill -f "socat TCP-LISTEN" 2>/dev/null; sleep 0.5; socat TCP-LISTEN:1088,fork,reuseaddr TCP:127.0.0.1:1087 & socat TCP-LISTEN:1089,fork,reuseaddr TCP:127.0.0.1:1082 & echo "✅ Proxy started: 1088→1087, 1089→1082"'

# 停止代理转发
alias proxy-stop='pkill -f "socat TCP-LISTEN" && echo "🛑 Proxy stopped"'

# 查看代理状态
alias proxy-status='echo "--- 1088 ---"; lsof -iTCP:1088 -sTCP:LISTEN 2>/dev/null; echo "--- 1089 ---"; lsof -iTCP:1089 -sTCP:LISTEN 2>/dev/null'

# 测试代理是否可用
alias proxy-test='curl -x http://127.0.0.1:1088 -I https://www.google.com --connect-timeout 5 -s -o /dev/null -w "HTTP %{http_code}\n"'



# [2026-09-24 MacPorts接管] 已注释brew shellenv，回滚: 取消下行注释即可
# eval "$(/usr/local/bin/brew shellenv zsh)"
export PYENV_ROOT="$HOME/.pyenv"
export HOMEBREW_MAKE_JOBS=10
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh)"
eval "$(pyenv virtualenv-init -)"
# [2026-09-23 MacPorts迁移] 原brew路径: /usr/local/etc/profile.d/autojump.sh
[[ -f /opt/local/etc/profile.d/autojump.sh ]] && . /opt/local/etc/profile.d/autojump.sh

export OLLAMA_NUM_GPU=999

# opencode
export PATH=$HOME/.opencode/bin:$PATH


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('$HOME/miniforge3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/miniforge3/etc/profile.d/conda.sh" ]; then
        . "$HOME/miniforge3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/miniforge3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba shell init' !!
export MAMBA_EXE='$HOME/miniforge3/bin/mamba';
export MAMBA_ROOT_PREFIX='$HOME/miniforge3';
__mamba_setup="$("$MAMBA_EXE" shell hook --shell zsh --root-prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    alias mamba="$MAMBA_EXE"  # Fallback on help from mamba activate
fi
unset __mamba_setup
# <<< mamba initialize <<<

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME/bin:"*) ;;
  *) export PATH="$PNPM_HOME/bin:$PATH" ;;
esac
# pnpm end

# [2026-09-23 MacPorts迁移] 原brew路径: /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
# [2026-09-23 MacPorts迁移] 原brew路径: /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /opt/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
eval "$(atuin init zsh)"
eval "$(fzf --zsh)"


alias p="pnpm"
export PATH="$PNPM_HOME:$PATH"
. "$HOME/.local/bin/env"

# [2026-09-24] fnm 必须放最后：它的 PATH 需要压过 ~/.local/bin/node（hermes 自带的 v22.23.1），
# 否则项目里的 .nvmrc 不会生效，cd 进目录也无法自动切换 Node 版本。
# 回滚：删掉下面这行，并恢复文件开头的两行注释。
# 显式带 --shell zsh，避免非交互/嵌套 shell 场景下 fnm 无法推断 shell 而静默失效。
eval "$(fnm env --use-on-cd --shell zsh)"
