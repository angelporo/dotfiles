#!/usr/bin/env bash
# ==============================================================================
# 把 dotfiles 里的「个人 Rime 配置」覆盖到 ~/Library/Rime
#
# 这是一层「个人覆盖层」：官方 rime-ice 发行版自带的 default.yaml / *.schema.yaml /
# lua/ / dicts/ 等都不在这里，只放你自己的 *.custom.yaml 与个人短语、用户词库快照。
# 新机器流程：先装官方 rime-ice（会下载 420MB 语法模型）→ 跑本脚本 → 注销重登。
#
# 用法：
#   bash ~/dotfiles/rime/apply.sh          # 应用个人配置 + 恢复用户词库（幂等，已相同则跳过）
#   DRY=1 bash ~/dotfiles/rime/apply.sh    # 只预览将做什么，不改动
#   CAPTURE=1 bash ~/dotfiles/rime/apply.sh  # 把本机已学词库抓回 dotfiles 快照（换机前刷新用）
#
# 安全：每次覆盖前，会把 ~/Library/Rime 里被替换的原文件备份到
#       ~/Library/Rime/.dotfiles-bak-<时间戳>/，可随时回退。
# 不主动拉起 Squirrel —— 输入法由 macOS 在你点输入框时自动接管。
# ==============================================================================
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC="$HERE/config"
DEST="$HOME/Library/Rime"
DRY="${DRY:-0}"
CAPTURE="${CAPTURE:-0}"

# 机器专属 / 运行时文件，绝不复制
EXCLUDE=(installation.yaml)

if [ ! -d "$DEST" ]; then
  echo "错误：未找到 $DEST"
  echo "请先安装官方 rime-ice + 鼠须管(Squirrel) 输入法，再运行本脚本。"
  exit 1
fi

# ------------------------------------------------------------------------------
# CAPTURE 模式：把本机已学词库抓回 dotfiles 快照
# ------------------------------------------------------------------------------
capture_userdb() {
  local s="$DEST/rime_ice.userdb"
  local d="$HERE/userdb/rime_ice.userdb"
  if [ ! -d "$s" ]; then
    echo "错误：未找到 $s（请先至少用一次 rime_ice 输入法，产生用户词库）"
    exit 1
  fi
  if [ -d "$d" ]; then
    local bak="$HERE/userdb/.bak-$(date +%Y%m%d-%H%M%S)"
    mv "$d" "$bak"
    echo "  备份旧快照: $bak"
  fi
  cp -a "$s" "$d"
  echo "  已抓取用户词库快照 -> $d"
  echo "  ★ 别忘了 git add 并提交，快照才算纳入同步。"
}

if [ "$CAPTURE" = "1" ]; then
  capture_userdb
  exit 0
fi

# ------------------------------------------------------------------------------
# 应用模式：复制 config/* + 恢复用户词库快照
# ------------------------------------------------------------------------------
BAK="$DEST/.dotfiles-bak-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$BAK"

applied=0
skipped=0

for f in "$SRC"/*; do
  [ -f "$f" ] || continue
  bn="$(basename "$f")"

  # 跳过排除清单
  skip=0
  for ex in "${EXCLUDE[@]}"; do
    [ "$bn" = "$ex" ] && skip=1 && break
  done
  [ "$skip" = "1" ] && { echo "  跳过(排除): $bn"; continue; }

  dst="$DEST/$bn"

  # 已相同则跳过（幂等）
  if [ -f "$dst" ] && diff -q "$f" "$dst" >/dev/null 2>&1; then
    echo "  已是最新(跳过): $bn"
    skipped=$((skipped + 1))
    continue
  fi

  if [ "$DRY" = "1" ]; then
    echo "  [DRY] 将复制: $bn -> $dst"
    continue
  fi

  # 备份已存在的目标文件
  if [ -f "$dst" ]; then
    cp -a "$dst" "$BAK/$bn"
    echo "  备份原文件: $BAK/$bn"
  fi
  cp -a "$f" "$dst"
  echo "  已应用: $bn"
  applied=$((applied + 1))
done

# ------------------------------------------------------------------------------
# 用户词库快照恢复（换机迁移学习成果）
#   仅在目标词库不存在或为空时复制，避免覆盖本机已学词。
#   注意：请在 Squirrel 未运行时执行；若已运行，重登后再部署更稳妥。
# ------------------------------------------------------------------------------
USERDB_SRC="$HERE/userdb/rime_ice"
USERDB_DST="$DEST/rime_ice.userdb"
if [ -d "$USERDB_SRC" ]; then
  has_ldb=0
  if [ -d "$USERDB_DST" ] && ls "$USERDB_DST"/*.ldb >/dev/null 2>&1; then
    has_ldb=1
  fi
  if [ "$has_ldb" = "1" ]; then
    echo "  已存在用户词库，未覆盖（保留本机学习成果）"
    echo "  如需用快照覆盖：先退出 Squirrel，删除 ~/Library/Rime/rime_ice.userdb 后重跑本脚本"
  elif [ "$DRY" = "1" ]; then
    echo "  [DRY] 将恢复用户词库快照到 $USERDB_DST"
  else
    cp -a "$USERDB_SRC" "$USERDB_DST"
    echo "  已恢复用户词库到 $USERDB_DST"
  fi
else
  echo "  无用户词库快照($USERDB_SRC)，跳过"
fi

echo ""
if [ "$DRY" = "1" ]; then
  echo "[DRY] 预览结束，未做任何修改。"
else
  echo "完成：已应用 $applied 个文件，跳过 $skipped 个（已是最新）。"
  echo ""
  echo "★ 关键一步：复制完成后请【注销并重新登录】。"
  echo "  否则 macOS 输入法框架里已打开的 App（微信/IDEA 等）会话是过期的，会打不了中文。"
  echo "  这是系统行为，不是配置问题。重登后若仍未生效，到菜单栏输入法图标切到『鼠须管』再切回即可。"
  echo ""
  echo "如需回退：把 $BAK/ 里的对应文件复制回 ~/Library/Rime 即可。"
fi
