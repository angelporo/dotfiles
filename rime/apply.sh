#!/usr/bin/env bash
# ==============================================================================
# 把 dotfiles 里的「个人 Rime 配置」覆盖到 ~/Library/Rime
#
# 支持两套方案，目录严格区分：
#   ice   -> ice/config/      + ice/userdb/rime_ice/      (rime-ice + 小鹤双拼 + 万象语法)
#   frost -> frost/config/    + frost/userdb/rime_frost/  (白霜拼音 + 小鹤双拼 + 万象大模型)
#
# 每套方案只放你自己的「个人覆盖层」：*.custom.yaml、个人短语、用户词库快照。
# 官方文件(default.yaml / *.schema.yaml / lua/ / dicts/ / *.gram 等) 不在本目录，由官方安装提供。
#
# 新机器流程：先装官方方案(ice 或 万象) + 鼠须管(Squirrel) -> 跑本脚本 -> 注销重登。
#
# 用法：
#   bash apply.sh ice            # 应用 ice 个人配置 + 恢复 ice 用户词库
#   bash apply.sh frost          # 应用白霜个人配置 + 恢复白霜用户词库
#   DRY=1 bash apply.sh <方案>   # 只预览，不改动
#   CAPTURE=1 bash apply.sh <方案>  # 把本机已学词库抓回 dotfiles 快照
#
# 安全机制：
#   - 覆盖前原文件备份到 ~/Library/Rime/.dotfiles-bak-<时间戳>/
#   - 写入 ~/Library/Rime/.dotfiles-flavor 标记当前方案；
#     误把 A 套方案应用到 B 套机器时，会拒绝并要求 FORCE=1，避免破坏当前输入法。
# 不主动拉起 Squirrel —— 输入法由 macOS 在你点输入框时自动接管。
# ==============================================================================
set -o pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEST="$HOME/Library/Rime"
DRY="${DRY:-0}"
CAPTURE="${CAPTURE:-0}"

usage() {
  cat <<'EOF'
用法：
  bash apply.sh <ice|frost>              应用某套方案的个人配置 + 恢复用户词库
  DRY=1 bash apply.sh <ice|frost>        只预览，不改动
  CAPTURE=1 bash apply.sh <ice|frost>    把本机已学词库抓回 dotfiles 快照
EOF
}

FLAVOR="${1:-}"
if [ -z "$FLAVOR" ]; then
  echo "错误：请指定方案 ice 或 wanxiang"
  usage
  exit 1
fi

case "$FLAVOR" in
  ice)
    CONF_DIR="$HERE/ice/config"
    UDB_NAME="rime_ice"
    SNAP="$HERE/ice/userdb/rime_ice"
    ;;
  frost)
    CONF_DIR="$HERE/frost/config"
    UDB_NAME="rime_frost"
    SNAP="$HERE/frost/userdb/rime_frost"
    ;;
  *)
    echo "错误：未知方案 '$FLAVOR'（可选 ice | frost）"
    usage
    exit 1
    ;;
esac

# 机器专属 / 运行时文件，绝不复制
EXCLUDE=(installation.yaml)

if [ ! -d "$DEST" ]; then
  echo "错误：未找到 $DEST"
  echo "请先安装官方方案(ice 或 万象) + 鼠须管(Squirrel) 输入法，再运行本脚本。"
  exit 1
fi

# ------------------------------------------------------------------------------
# 方案一致性守卫：避免把 A 套方案误覆盖到 B 套机器的 ~/Library/Rime
# ------------------------------------------------------------------------------
MARKER="$DEST/.dotfiles-flavor"

# 探测当前 ~/Library/Rime 实际在用的方案
detect_live_flavor() {
  # 1) 本脚本上次写入的标记（最可靠）
  [ -f "$MARKER" ] && { cat "$MARKER"; return; }
  # 2) 看用户词库目录：哪套的 .userdb 存在即用哪套（二者皆无则交由第 3 步）
  local f=0
  local i=0
  [ -d "$DEST/rime_frost.userdb" ] && f=1
  [ -d "$DEST/rime_ice.userdb" ] && i=1
  if [ "$f" = 1 ] && [ "$i" = 0 ]; then echo "frost"; return; fi
  if [ "$i" = 1 ] && [ "$f" = 0 ]; then echo "ice"; return; fi
  # 3) 退回 default.yaml 的首条 schema_list（新装机器尚未产生 userdb 时）
  local s
  s="$(grep -m1 -A2 'schema_list' "$DEST/default.yaml" 2>/dev/null | grep -oE 'schema: [a-z_]+' | head -1 | awk '{print $2}')"
  case "$s" in
    rime_frost_double_pinyin_flypy) echo "frost" ;;
    double_pinyin_flypy|rime_ice) echo "ice" ;;
    *) echo "" ;;
  esac
}

live_flavor="$(detect_live_flavor)"
block=0
reason=""
if [ -n "$live_flavor" ] && [ "$live_flavor" != "$FLAVOR" ] && [ "${FORCE:-0}" != "1" ]; then
  block=1
  reason="检测到当前 ~/Library/Rime 正在使用「$live_flavor」方案（依据 userdb / default.yaml），与你要应用的「$FLAVOR」不一致。"
fi

if [ "$block" = "1" ] && [ "${FORCE:-0}" != "1" ]; then
  echo "WARNING: 拒绝应用：$reason"
  echo "    两套方案共用 ~/Library/Rime，混用会破坏当前输入法。请确认操作："
  echo "    1) 若确实要切换到 $FLAVOR："
  echo "       a. 备份当前： mv ~/Library/Rime ~/Library/Rime.bak-$(date +%Y%m%d)-$live_flavor"
  echo "       b. 用官方方式准备好 $FLAVOR 的 ~/Library/Rime（装官方方案 + 首次部署）"
  echo "       c. 再重跑： bash apply.sh $FLAVOR"
  echo "    2) 若只是误操作：请运行  bash apply.sh $live_flavor"
  echo "    3) 仍要强制覆盖（会冲掉当前方案的配置）： FORCE=1 bash apply.sh $FLAVOR"
  exit 1
fi

# ------------------------------------------------------------------------------
# CAPTURE 模式：把本机已学词库抓回 dotfiles 快照
# ------------------------------------------------------------------------------
capture_userdb() {
  local s="$DEST/$UDB_NAME.userdb"
  local d="$SNAP"
  if [ ! -d "$s" ]; then
    echo "错误：未找到 $s（请先至少用一次 $FLAVOR 输入法，产生用户词库）"
    exit 1
  fi
  if [ -d "$d" ]; then
    local bak="$HERE/$FLAVOR/userdb/.bak-$(date +%Y%m%d-%H%M%S)"
    mv "$d" "$bak"
    echo "  备份旧快照: $bak"
  fi
  cp -a "$s" "$d"
  echo "  已抓取 $FLAVOR 用户词库快照 -> $d"
  echo "  ★ 别忘了 git add 该目录并提交，快照才算纳入同步。"
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

if [ ! -d "$CONF_DIR" ]; then
  echo "错误：未找到 $CONF_DIR"
  exit 1
fi

applied=0
skipped=0

for f in "$CONF_DIR"/*; do
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
#   注意：请在 Squirrel 未运行时执行；若已运行，注销重登后再部署更稳妥。
# ------------------------------------------------------------------------------
USERDB_SRC="$SNAP"
USERDB_DST="$DEST/$UDB_NAME.userdb"
if [ -d "$USERDB_SRC" ]; then
  has_ldb=0
  if [ -d "$USERDB_DST" ] && ls "$USERDB_DST"/*.ldb >/dev/null 2>&1; then
    has_ldb=1
  fi
  if [ "$has_ldb" = "1" ]; then
    echo "  已存在用户词库，未覆盖（保留本机学习成果）"
    echo "  如需用快照覆盖：先退出 Squirrel，删除 ~/Library/Rime/$UDB_NAME.userdb 后重跑本脚本"
  elif [ "$DRY" = "1" ]; then
    echo "  [DRY] 将恢复用户词库快照到 $USERDB_DST"
  else
    rm -rf "$USERDB_DST"
    cp -a "$USERDB_SRC" "$USERDB_DST"
    echo "  已恢复用户词库到 $USERDB_DST"
  fi
else
  echo "  无用户词库快照($USERDB_SRC)，跳过"
fi

# 写入 flavor 标记
if [ "$DRY" != "1" ]; then
  echo "$FLAVOR" > "$MARKER"
fi

echo ""
if [ "$DRY" = "1" ]; then
  echo "[DRY] 预览结束，未做任何修改。"
else
  echo "完成：已应用 $applied 个文件，跳过 $skipped 个（已是最新）。方案标记：$FLAVOR"
  echo ""
  echo "★ 关键一步：复制完成后请【注销并重新登录】。"
  echo "  否则 macOS 输入法框架里已打开的 App（微信/IDEA 等）会话是过期的，会打不了中文。"
  echo "  重登后若未生效，到菜单栏输入法图标切到『鼠须管』再切回即可。"
  echo ""
  echo "如需回退：把 $BAK/ 里的对应文件复制回 ~/Library/Rime 即可。"
fi
