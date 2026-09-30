# shell 配置

软链到 home 目录的 shell 启动文件。由 `../setup.sh` 统一管理。

## 文件说明

| 文件 | 作用 |
|---|---|
| `.zshrc` | zsh 主配置（Oh My Zsh、pyenv、conda/mamba、pnpm、fnm、atuin、fzf、代理别名等） |
| `.zprofile` | 登录 shell 配置（MacPorts PATH、OrbStack、Hermes 的 ~/.local/bin） |
| `.bash_profile` | bash 登录配置（zx 迁移相关） |
| `.profile` | POSIX 通用配置 |

## 维护

```bash
# 改完仓库里的 .zshrc 后，新开终端即生效（软链，无需复制）
# 导出当前机器最新状态（如果你直接改了 ~/.zshrc 而非仓库）：
cp ~/.zshrc ~/dotfiles/shell/.zshrc
# 把硬编码家目录路径换成 $HOME 再提交
```

## 注意

- `.zshrc` 里的路径已经参数化为 `$HOME`，换机器可直接用。
- 依赖项（需目标机器已装）：MacPorts 的 zsh-autosuggestions / zsh-syntax-highlighting、
  pyenv、miniforge3、pnpm、fnm、atuin、fzf、Oh My Zsh。
  这些不在 dotfiles 内，需另装。
