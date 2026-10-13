# rime（鼠须管 / Squirrel 个人配置）

本目录只存放**个人覆盖层**，不包含官方发行版文件。目录下分两套方案，**严格区分、互不干扰**：

```
rime/
├── ice/                        # rime-ice 方案（雾凇 + 小鹤双拼 + 万象语法模型）
│   ├── config/                 # 个人 *.custom.yaml / 短语 / user.yaml
│   └── userdb/rime_ice/        # 用户词库快照（频率浮动学习成果）
├── wanxiang/                   # 万象拼音方案（万象 + 小鹤双拼 + 声调选词）
│   ├── config/                 # 个人 wanxiang.custom.yaml / 短语 / user.yaml
│   └── userdb/wanxiang/        # 用户词库快照（含声调选词学习成果）
├── apply.sh                    # 部署脚本（需指定 ice 或 wanxiang）
├── .gitignore
└── README.md
```

- **ice 组合**：官方 rime-ice 发行版 + 小鹤双拼(`double_pinyin_flypy`) + 万象语法模型(`wanxiang-lts-zh-hans`)。
- **wanxiang 组合**：官方万象拼音(`rime_wanxiang`) + 小鹤双拼 + 原生声调选词。
  相比 ice 多了「拼音后补声调数字筛选同音字」的手感，整句/长词能力同源。

> 官方文件（default.yaml、*.schema.yaml、lua/、dicts/、opencc/、*.gram 等）由官方仓库提供；
> 这里只放你自己在官方之上叠加的配置、个人短语，以及**用户词库快照**。

---

## `ice/config/` 文件

| 文件 | 作用 |
|---|---|
| `default.custom.yaml` | 全局覆盖：语法模型参数、模糊音(z/zh、in/ing)、emacs 键位、中英切换(Ctrl+. )、候选拼音注释关 |
| `double_pinyin_flypy.custom.yaml` | 小鹤双拼方案：启用用户词典+短语浮动、移除表情与拆字反查 |
| `rime_ice.custom.yaml` | rime_ice 全拼方案覆盖 |
| `melt_eng.custom.yaml` | 英文方案 |
| `radical_pinyin.custom.yaml` | 拼音反查 recipe |
| `squirrel.custom.yaml` | 鼠须管前端：字体、候选样式、配色 |
| `custom_phrase.txt` / `custom_phrase_double.txt` / `custom_phrase.dict.yaml` | 个人自定义短语（邮箱、手机号、常用语） |
| `user.yaml` | 当前选中方案与访问时间（可选，Squirrel 也会自动重建） |

## `wanxiang/config/` 文件

| 文件 | 作用 |
|---|---|
| `wanxiang.custom.yaml` | 万象个性化补丁：小鹤双拼、模糊音(z/zh、c/ch、s/sh、in/ing、en/eng)、语法收敛(collocation 5/3)、关整句学词、候选数 5、emacs 键位、Ctrl+. 中英切换 |
| `squirrel.custom.yaml` | 鼠须管前端：字体、候选样式、配色（与 ice 同款外观） |
| `custom_phrase.dict.yaml` | 个人自定义短语（万象动态格式化版） |
| `user.yaml` | 当前选中方案（可选） |

> 两套方案的 `*.custom.yaml` 均**不含机器专属绝对路径**，可安全跨机器使用。

---

## 用户词库快照

开启「频率浮动 / 用户词典」后，学过的词会写进 `~/Library/Rime/<方案>.userdb/`（leveldb）：

- **ice** → `~/Library/Rime/rime_ice.userdb/`  → 快照存 `ice/userdb/rime_ice/`
- **wanxiang** → `~/Library/Rime/wanxiang.userdb/` → 快照存 `wanxiang/userdb/wanxiang/`

换机时若想带上这些学习成果，这里各保存一份**快照**，`apply.sh` 部署时恢复到新机器。

- 刷新快照（本机又学了很多词后）：
  ```bash
  CAPTURE=1 bash ~/dotfiles/rime/apply.sh ice
  CAPTURE=1 bash ~/dotfiles/rime/apply.sh wanxiang
  ```
  会把对应 `~/Library/Rime/*.userdb/` 抓回快照目录，记得 `git add` 提交。
- 恢复时机：`apply.sh` 只在**目标词库不存在或为空**时复制，避免覆盖新机器上已有的学习数据。
  请在 Squirrel 未运行时执行；若已运行，注销重登后再跑更稳妥。
- 快照目录命名特意**不带 `.userdb` 后缀**，因此不会被 `*.userdb/` 规则忽略，自动纳入同步。

---

## 刻意不包含（由官方安装提供或运行时生成）

- `wanxiang-lts-zh-hans.gram`（420MB 语法模型）：官方安装时下载，体积大，不入 git。
- `build/`：编译缓存，自动生成。
- `*.userdb*`：`~/Library/Rime/` 下运行时产生的用户词库**不提交**；
  但本目录内的 **`ice/userdb/rime_ice/` 与 `wanxiang/userdb/wanxiang/` 快照是迁移用的，会提交**。
- `installation.yaml`：含机器唯一 ID 与 `/Users/liyuan` 路径，**不复制**。
- 官方 `default.yaml`、`*.schema.yaml`、`cn_dicts/`、`dicts/`、`en_dicts/`、`lua/`、`opencc/`、`others/`、`Rime/` 等：来自官方仓库。
- `custom/` 目录（旧万象方案文件）：已切回 ice/万象官方，不再使用，未纳入。

---

## 换机迁移步骤

```bash
# 1) 安装鼠须管(Squirrel) + 官方方案（ice 或 万象）
#    ice：按 rime-ice 官方文档安装并首次部署（会自动下载 420MB 语法模型）
#    wanxiang：按 rime_wanxiang 官方文档安装并首次部署

# 2) 应用你的个人配置（二选一，务必与第 1 步对应）
bash ~/dotfiles/rime/apply.sh ice        # 用 ice 配置
bash ~/dotfiles/rime/apply.sh wanxiang   # 用万象配置

# 3) ★ 注销并重新登录（必须）
#    否则已打开的 App 输入法会话是过期的，会打不了中文。
```

- 先预览会做什么： `DRY=1 bash ~/dotfiles/rime/apply.sh <ice|wanxiang>`
- 回退： `apply.sh` 每次覆盖前会把原文件备份到 `~/Library/Rime/.dotfiles-bak-<时间戳>/`，把对应文件复制回去即可。
- 安全守卫： `~/Library/Rime/.dotfiles-flavor` 记录当前方案。若误把 A 套配置应用到 B 套机器，
  `apply.sh` 会拒绝并要求 `FORCE=1`，避免破坏当前正在用的输入法。

## 注意

- `__patch:` recipe（rime_ice/melt_eng/radical_pinyin 的 custom 文件）由 rime 在**部署时**自动展开，
  无需单独跑安装脚本；只要官方 `others/recipes/`(ice) 或对应模块(wanxiang)在，复制后重部署即生效。
- 若某 App 打不了中文：先彻底退出该 App(⌘Q)重开；不行就注销重登。切勿用 `open` 手动拉起 Squirrel。
