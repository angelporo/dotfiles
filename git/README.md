# git 配置

| 文件 | 作用 |
|---|---|
| `.gitconfig` | 全局 git 配置（user.name / email） |
| `ignore` | 全局忽略规则，软链到 `~/.config/git/ignore`（git 2.x 自动读取） |

## 维护

```bash
# 改完仓库里的文件，新开终端即生效（软链）
# 如果直接改了 ~/.gitconfig，同步回来：
cp ~/.gitconfig ~/dotfiles/git/.gitconfig
```

> 注意：`.gitconfig` 里的 `user.email = 940079461@qq.com` 是个人信息，
> 若仓库对外公开请注意。
