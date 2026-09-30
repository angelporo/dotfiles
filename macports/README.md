# MacPorts 环境

brew 已卸载，本机统一用 MacPorts（`/opt/local/bin/port`）。

## 日常：保持清单最新

装了新软件之后：

```bash
cd ~/dotfiles/macports
./dump.sh --check   # 先看本机与仓库清单差在哪
./dump.sh           # 确认后导出，覆盖 ports-requested.txt
git add . && git commit -m "chore(macports): 更新端口清单"
```

`dump.sh` 只记录**主动安装**的端口（`port installed requested`），依赖不写进去，
恢复时 MacPorts 会自动带入。

它还会自动检测**非默认变体**：MacPorts 的变体列表里 `[+]xxx` 表示默认开启，
只有非默认的才需要额外记录（会生成 `ports-variants.txt`）。
目前本机 38 个端口用的全是默认变体，所以没有这个文件。

## 新机器：一键重建

```bash
# 1. 装 MacPorts 本体：https://www.macports.org/install.php
# 2.
cd ~/dotfiles/macports && ./restore.sh
```

`restore.sh` 会：selfupdate → 合并安装两份清单 → `port rev-upgrade` 自检。

## 常用维护命令

```bash
sudo port reclaim              # 清旧版本（brew 会自动做，port 不会，会一直涨）
port variants <pkg>            # 看编译选项
port select --list python      # 多版本切换
port provides /opt/local/bin/x # 反查文件属于哪个 port
```

macOS 大版本升级后（MacPorts 2.10+）：

```bash
xcode-select --install
sudo port migrate              # 自动快照 + 重装不兼容端口
sudo port restore --last       # 有失败就修好后重来
port snapshot --list           # 清理快照
```

## 注意事项

- **GUI 应用不走 port**（MacPorts 没有 cask 机制），从官网或 App Store 装。
- **JS 生态工具（eslint / prettier / vite）一律走 npm/pnpm**，避免与项目 devDependencies 版本打架。
- Python 若已有 pyenv / conda，**不要执行 `port select --set python`**。
- 装东西遇到 `Failed to checksum`，多半是源码下载被镜像掐断，
  手动把文件放进 `/opt/local/var/macports/distfiles/<port>/` 再重装即可。
