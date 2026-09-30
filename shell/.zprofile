
# [2026-09-24 MacPorts接管] 已注释brew shellenv，回滚: 取消下行注释即可
# eval "$(/usr/local/bin/brew shellenv zsh)"

# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

# Hermes Agent — ensure ~/.local/bin is on PATH
export PATH="$HOME/.local/bin:$PATH"

##
# 迁移 MacPorts 时，原 .zprofile 备份为 ~/.zprofile.macports-saved_2026-09-23_at_16:23:43
##

# MacPorts Installer addition on 2026-09-23_at_16:23:43: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

