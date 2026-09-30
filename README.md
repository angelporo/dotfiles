# macbookpro 16

dotfiles 管理说明：配置存放在各组件目录，由 `setup.sh` 软链到 home 目录，改仓库即改生效配置。

```bash
bash ~/dotfiles/setup.sh            # 链接全部（shell / git / rime）
bash ~/dotfiles/setup.sh shell      # 只链接 shell
bash ~/dotfiles/setup.sh git        # 只链接 git
bash ~/dotfiles/setup.sh rime       # 调用 rime/link.sh
```

## 组件

| 目录 | 内容 |
|---|---|
| `shell/` | `.zshrc` `.zprofile` `.bash_profile` `.profile`（路径已参数化为 `$HOME`） |
| `git/` | `.gitconfig` `ignore`（全局忽略，软链到 `~/.config/git/ignore`） |
| `rime/` | 万象拼音 Base + 小鹤双拼（`install.sh` / `link.sh` / `update.sh`） |
| `macports/` | `ports-requested.txt` + `dump.sh` / `restore.sh` / `README.md` |
| `centaur-emacs/` | Emacs 配置 |
| `etc/` | hosts 等系统文件 |

## 注意

- `git/.gitconfig` 含个人邮箱 `940079461@qq.com`，对外公开仓库前请注意。
- `rime-async` / `rime-async-wanxiang` 为 Rime 词频同步目录（含个人输入习惯），目前已被 git 跟踪；
  若仓库公开，建议 `git rm --cached` 并从 `.gitignore` 排除。
- 仓库历史含 `rime-08dd95f-macOS/dist`（librime 二进制），是 `.git` 体积偏大的主因。

## emacs

使用[centaur emacs ](https://github.com/seagle0128/.emacs.d) 配置
