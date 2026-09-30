# rime（鼠须管 / rime-ice 个人配置）

本目录只存放**个人覆盖层**，不包含官方 rime-ice 发行版文件。

你的完整组合是：**rime-ice 发行版 + 小鹤双拼(double_pinyin_flypy) + 万象语法模型(wanxiang-lts-zh-hans)**。
官方文件（default.yaml、*.schema.yaml、lua/、dicts/、opencc/ 等）由官方仓库提供；
这里只放你自己在官方之上叠加的 `*.custom.yaml` 与个人短语。

## config/ 包含的文件

| 文件 | 作用 |
|---|---|
| `default.custom.yaml` | 全局覆盖：万象语法模型参数、模糊音(z/zh、in/ing)、emacs 键位、中英切换(Ctrl+. )、候选拼音注释关 |
| `double_pinyin_flypy.custom.yaml` | 小鹤双拼方案覆盖：启用万象语法、长词优先、拼写设定 |
| `rime_ice.custom.yaml` | rime_ice 全拼方案覆盖：启用万象语法 recipe |
| `melt_eng.custom.yaml` | 英文方案：双拼音译衔接 recipe |
| `radical_pinyin.custom.yaml` | 五笔拼音反查 recipe |
| `squirrel.custom.yaml` | 鼠须管前端：字体(PingFangSC)、候选样式、配色等 |
| `custom_phrase.txt` / `custom_phrase_double.txt` / `custom_phrase.dict.yaml` | 个人自定义短语（邮箱、手机号、常用语等） |
| `user.yaml` | 当前选中的方案(double_pinyin_flypy)与访问时间（可选，Squirrel 也会自动重建） |

> 这些文件均**不含机器专属绝对路径**，可安全跨机器使用。

## 刻意不包含（由官方安装提供或运行时生成）

- `wanxiang-lts-zh-hans.gram`（420MB 语法模型）：官方安装时下载，体积大，不入 git。
- `build/`：编译缓存，自动生成。
- `*.userdb*`：个人词库（学习到的字词），运行时数据，不提交。
- `installation.yaml`：含机器唯一 ID 与 `/Users/liyuan` 路径，**不复制**。
- 官方 `default.yaml`、`*.schema.yaml`、`cn_dicts/`、`dicts/`、`en_dicts/`、`lua/`、`opencc/`、`others/`、`Rime/` 等：来自官方仓库。
- `custom/` 目录（旧万象方案文件）：你已切回 ice，不再使用，未纳入。

## 换机迁移步骤

```bash
# 1) 安装鼠须管(Squirrel) 输入法 + 官方 rime-ice
#    （官方安装会下载 420MB 语法模型并首次部署；按 rime-ice 官方文档执行）

# 2) 应用你的个人配置
bash ~/dotfiles/rime/apply.sh

# 3) ★ 注销并重新登录（必须）
#    否则已打开的 App 输入法会话是过期的，会打不了中文。
```

- 想先看会做什么： `DRY=1 bash ~/dotfiles/rime/apply.sh`
- 回退： `apply.sh` 每次覆盖前会把原文件备份到 `~/Library/Rime/.dotfiles-bak-<时间戳>/`，把对应文件复制回去即可。

## 注意

- `__patch:` recipe（rime_ice/melt_eng/radical_pinyin 的 custom 文件）由 rime-ice 在**部署时**自动展开，
  无需单独跑安装脚本；只要官方 `others/recipes/` 在，复制后重部署即生效。
- 若某 App 打不了中文：先彻底退出该 App(⌘Q)重开；不行就注销重登。切勿用 `open` 手动拉起 Squirrel。

## 历史遗留：`wanxiang/` 子目录

里面是早期「整套切换万象 Base 方案」时期的脚本(install.sh/link.sh/update.sh)与配置，
现已切回 rime-ice，这些不再使用，仅作存档保留，不参与迁移。
