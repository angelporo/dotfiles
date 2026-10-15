# macbookpro 16

dotfiles 管理说明：配置存放在各组件目录，由 `setup.sh` 软链到 home 目录，改仓库即改生效配置。

## 全新机器一键迁移（推荐）

换新 Mac 后，克隆仓库下来直接跑引导脚本，它会按依赖顺序把环境完整搭起来：

```bash
git clone <你的仓库> ~/dotfiles
bash ~/dotfiles/bootstrap.sh            # 全流程：CLT→MacPorts→端口→Squirrel→Rime个人配置→软链→hosts备份→表情屏蔽
DRY=1 bash ~/dotfiles/bootstrap.sh      # 只看会做什么，不真改
bash ~/dotfiles/bootstrap.sh ports      # 只想重装端口清单时
```

引导脚本会自动：装 Xcode 命令行工具、按 macOS 大版本装对应 MacPorts pkg、装 38 个端口（含 emacs）、
装 Squirrel 输入法、套用 Rime 个人配置（apply.sh，前提：官方 rime-ice 已先装到 ~/Library/Rime）、
软链全部 dotfiles（shell/git/rime/emacs/alfred）、备份 /etc/hosts、屏蔽 ⌃⌘空格 表情弹框。
装完注销重登一次让输入法与表情屏蔽生效；Squirrel 需到「系统设置→键盘→输入法」手动添加「鼠须管」。

## 只迁移配置（前置已就绪时）

```bash
bash ~/dotfiles/setup.sh            # 链接全部（shell / git / config / ssh / rime / centaur-emacs / alfred）
bash ~/dotfiles/setup.sh shell      # 只链接 shell
bash ~/dotfiles/setup.sh git        # 只链接 git
bash ~/dotfiles/setup.sh config     # 软链小工具配置（ghostty/karabiner/atuin 等）到 ~/.config
bash ~/dotfiles/setup.sh ssh        # 软链 ssh config（私钥不入库）
bash ~/dotfiles/setup.sh rime       # 调用 rime/apply.sh（套用个人配置到 ~/Library/Rime）
bash ~/dotfiles/setup.sh emacs      # 软链 centaur-emacs 自定义层到 ~/.emacs.d
```

## 组件

| 目录 | 内容 |
|---|---|
| `shell/` | `.zshrc` `.zprofile` `.zshenv` `.bash_profile` `.profile`（路径已参数化为 `$HOME`） |
| `git/` | `.gitconfig` `ignore`（全局忽略，软链到 `~/.config/git/ignore`） |
| `rime/` | rime-ice 个人覆盖层（小鹤双拼 + 万象语法模型）：`config/*.custom.yaml` + `userdb/rime_ice/` 词库快照 + `apply.sh`（flavor 仅 ice，frost 已于 2026-10-15 删除，git 历史可找回） |
| `config/` | 小工具配置（软链回 `~/.config/`）：`ghostty/` `linearmouse/` `atuin/` `uv/` `karabiner/karabiner.json` `shell_gpt/`（config+functions+roles，`.sgptrc` 含 API key 不入库）`opencode/`（json+plugins，node_modules 由 npm install 重建） |
| `ssh/` | `config`（主机别名，软链到 `~/.ssh/config`）；私钥绝不入 git |
| `macports/` | `ports-requested.txt`（47 端口）+ `dump.sh` / `restore.sh` / `README.md` |
| `centaur-emacs/` | Emacs 自定义层（custom.el / snippets 等），软链进 `~/.emacs.d`；核心需另装 |
| `alfred/` | Alfred 同步文件夹 `Alfred.alfredpreferences`（含 26 个工作流+主题+偏好），软链到 `~/Library/Application Support/Alfred/` |
| `etc/` | hosts 等系统文件（bootstrap 仅备份，不自动覆盖） |

## 注意

- `git/.gitconfig` 含个人邮箱 `940079461@qq.com`，对外公开仓库前请注意。
- `config/shell_gpt/` 不含 `.sgptrc`（内有 `OPENAI_API_KEY`），该文件只存在于本机 `~/.config/shell_gpt/.sgptrc`。
- `~/.ssh/` 私钥（id_ed25519/id_rsa）不入库，换机后从 1Password/离线备份恢复。
- Rime 个人配置与词库统一在 `rime/`（flavor：`ice`），词库快照 `rime/ice/userdb/rime_ice/` 已被 git 跟踪；
  若仓库公开，建议 `git rm --cached` 并从 `.gitignore` 排除。
- 历史上的 `rime-async` / `rime-async-wanxiang` 同步目录已于 2026-10-14 删除（旧 installation id `macbookpro-angel` 的同步快照，重装后不再使用；需要时从 git 历史找回）。
- 仓库历史含 `rime-08dd95f-macOS/dist`（librime 二进制），是 `.git` 体积偏大的主因。

## emacs

使用[centaur emacs ](https://github.com/seagle0128/.emacs.d) 配置
