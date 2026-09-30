#!/usr/bin/env bash
# 更新万象拼音，保留个人配置。
#
#   bash update.sh                      # 更新到最新版
#   WX_VERSION=18.1.0 bash update.sh    # 指定版本（也可用来回滚到旧版）
#   UPDATE_GRAM=1 bash update.sh        # 连 420MB 语法模型一起更新
#   FORCE=1 bash update.sh              # 版本号相同也强制重装
#
# 原理：个人配置在 dotfiles/rime/config/ 里（软链挂到 Rime 目录），
# 万象新版只覆盖它自己的文件，装完再让 link.sh 把软链重新挂回去。
set -euo pipefail

RIME_DIR="${RIME_DIR:-$HOME/Library/Rime}"
HERE="$(cd "$(dirname "$0")" && pwd)"
API="https://api.github.com/repos/amzxyz/rime-wanxiang/releases/latest"
GRAM_URL="https://github.com/amzxyz/RIME-LMDG/releases/download/LTS/wanxiang-lts-zh-hans.gram"
DEPLOYER="/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_deployer"

[[ -d "$RIME_DIR" ]] || { echo "✗ 找不到 $RIME_DIR，请先跑 install.sh"; exit 1; }

cur="$(tr -d '[:space:]' < "$RIME_DIR/version.txt" 2>/dev/null || echo "未知")"
echo "当前版本：$cur"

if [[ -z "${WX_VERSION:-}" ]]; then
  # 先把整个响应读进变量再解析：
  # 直接 curl | grep -m1 会因 grep 提前退出而关闭管道，导致 curl 报 "Failed writing body"
  release_json="$(curl -fsSL "$API" 2>/dev/null)" || release_json=""
  WX_VERSION="$(printf '%s' "$release_json" | grep -m1 '"tag_name"' | sed -E 's/.*"v?([^"]+)".*/\1/')"
  if [[ -z "$WX_VERSION" ]]; then
    echo "✗ 查不到最新版本号（可能是网络问题或 GitHub API 限流）"
    echo "  请手动指定：WX_VERSION=版本号 bash $0"
    exit 1
  fi
fi
echo "目标版本：$WX_VERSION"

if [[ "$cur" == "$WX_VERSION" && "${FORCE:-0}" != "1" ]]; then
  echo "已经是最新版，无事可做。（想重装加 FORCE=1）"
  exit 0
fi

# 1. 备份（APFS 克隆，几乎瞬间且不额外占空间）
bak="$HOME/Library/Rime.bak-preupdate-$(date +%Y%m%d%H%M)"
echo
echo "==> 1/5 备份到 $bak"
cp -aRc "$RIME_DIR" "$bak"

# 2. 下载
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
url="https://github.com/amzxyz/rime-wanxiang/releases/download/v${WX_VERSION}/rime-wanxiang-base.zip"
echo "==> 2/5 下载 v${WX_VERSION}"
curl -fsSL "$url" -o "$tmp/base.zip"
unzip -q "$tmp/base.zip" -d "$tmp/base"

# 3. 覆盖安装（会盖掉软链，下一步重建）
echo "==> 3/5 覆盖到 $RIME_DIR"
cp -R "$tmp/base/." "$RIME_DIR/"

# 4. 语法模型（默认不动，420MB 且更新不频繁）
echo "==> 4/5 语法模型"
if [[ "${UPDATE_GRAM:-0}" == "1" ]]; then
  curl -fsSL "$GRAM_URL" -o "$RIME_DIR/wanxiang-lts-zh-hans.gram"
  echo "    已更新"
else
  echo "    跳过（要更新加 UPDATE_GRAM=1）"
fi

# 5. 重建软链 + 预编译
echo "==> 5/5 重建配置软链"
bash "$HERE/link.sh"

if [[ -x "$DEPLOYER" ]]; then
  echo "    预编译部署产物…"
  "$DEPLOYER" --build "$RIME_DIR" "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" >/dev/null 2>&1 || true
fi

cat <<EOF

──────────────────────────────────────────
更新完成：$cur → $WX_VERSION

请做这两件事：
  1. 输入法菜单 →「重新部署」
  2. 抽验一遍（新版可能改字段名导致补丁失效）：
     · 打 vs7         应出「中」（小鹤双拼 + 一声筛选）
     · 打 zhen        应也能出「真」等（en/eng 模糊音）
     · Ctrl+.         中英切换
     · Ctrl+n / Ctrl+p  候选上下
     · 打 /wx         显示版本号，确认是 $WX_VERSION

出问题就还原：
  rm -rf $RIME_DIR
  mv $bak $RIME_DIR
  # 然后重新部署
──────────────────────────────────────────
EOF
