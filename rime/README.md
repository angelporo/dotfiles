# Rime 配置（万象拼音 Base + 小鹤双拼）

macOS / 鼠须管 Squirrel 1.1.2 / Rime 1.16.0

## 目录说明

```
rime/
├── config/
│   ├── wanxiang.custom.yaml   ← 核心配置（小鹤双拼、模糊音、快捷键、性能参数）
│   ├── squirrel.custom.yaml   ← 鼠须管皮肤、app_options
│   └── installation.yaml      ← 同步目录设置（含本机路径，换机器要改）
├── install.sh                 ← 全新机器一键重建
├── link.sh                    ← 把配置软链到 ~/Library/Rime（幂等）
└── .gitignore
```

`config/` 里的 `wanxiang.custom.yaml` 和 `squirrel.custom.yaml` 通过**软链**挂到
`~/Library/Rime/`，所以在 dotfiles 里改就是改生效中的配置，不用再手动复制。

```
~/Library/Rime/wanxiang.custom.yaml → ~/dotfiles/rime/config/wanxiang.custom.yaml
~/Library/Rime/squirrel.custom.yaml → ~/dotfiles/rime/config/squirrel.custom.yaml
```

## 新机器重建

```bash
# 1. 装好鼠须管（https://github.com/rime/squirrel/releases）
# 2. 跑脚本
bash ~/dotfiles/rime/install.sh

# 不要 420MB 语法模型（打字更快、整句变弱）
SKIP_GRAM=1 bash ~/dotfiles/rime/install.sh

# 3. 输入法菜单 →「重新部署」
```

换机器后记得改 `config/installation.yaml` 里的 `sync_dir`（默认指向
`/Users/liyuan/dotfiles/rime-async-wanxiang`，用户名不同要改）。

## 更新万象

```bash
bash ~/dotfiles/rime/update.sh              # 更新到最新版
WX_VERSION=18.1.0 bash ~/dotfiles/rime/update.sh   # 指定版本（也能用来回退）
UPDATE_GRAM=1 bash ~/dotfiles/rime/update.sh       # 连 420MB 语法模型一起更新
```

脚本会：查最新版 → **APFS 克隆备份**整个 Rime 目录 → 覆盖安装 → 重建软链 → 预编译。
跑完还要在输入法菜单点「重新部署」。

**为什么能放心更新**：个人配置在 `config/` 里，万象新版只覆盖它自己的文件，
软链会在第 5 步重新挂回去，两者不会互相覆盖。

**风险点**：新版可能改字段名，导致 `wanxiang.custom.yaml` 里的补丁失效。
所以每次更新后按脚本输出的清单抽验一遍（小鹤双拼、模糊音、快捷键）。
真出问题就按脚本末尾的还原命令恢复备份。

更新频率建议：词库更新比较频繁，一两个月一次即可；语法模型（420MB）更新很慢，
半年看一次就行。

## 脚本不会替你做的三件事

1. **词库 / 词频** —— 不在本仓库。老机器输入法菜单点「同步」导出到
   `rime-async-wanxiang`，新机器设好 `sync_dir` 后点「同步」+「重新部署」。
2. **屏蔽 ⌃⌘空格 表情弹框** —— 这是 macOS 系统级设置，不是 Rime 配置：
   ```bash
   defaults write -g NSUserKeyEquivalents -dict-add "表情与符号" '@~^E'
   ```
   注销重登后生效。撤销：`defaults delete -g NSUserKeyEquivalents`
3. **万象版本** —— 脚本固定装在 v18.0.15。想装新版：
   `WX_VERSION=新版本号 bash install.sh`

## 配置做了什么（相对万象默认）

| 项 | 默认 | 这里 |
|---|---|---|
| 输入方案 | 全拼 | 小鹤双拼 + 26 键 |
| 模糊音 | 全关 | z_zh / c_ch / s_sh / in_ing / en_eng（双向） |
| 语法模型搜索长度 | max 6 / min 2 | max 5 / min 3（更跟手） |
| 上屏按段学词 | `core_word_length: 4` | 0（关闭，上屏更快） |
| 候选数 | 6 | 5 |
| 手动排序置顶 | Ctrl+P | Ctrl+Shift+P（Ctrl+P 让给 emacs） |

快捷键追加了 14 条：`Ctrl+.` 中英切换、`Ctrl+,` 中英标点、`,` `.` 翻页、
emacs 的 `Ctrl+n/p/b/f/d/h/y/v`、`Alt+v`、`Ctrl+[`。

## 不进 git 的东西

`build/`（部署产物）、`wanxiang-lts-zh-hans.gram`（420MB）、`dicts/ lua/ opencc/`
（万象自带，release 可下载）、`*.userdb*`（个人词库）。

⚠️ 另外建议把仓库根的 `rime-async/`、`rime-async-wanxiang/` 也加进 .gitignore
—— 那是 Rime 的词频同步目录，里面是个人输入习惯。
