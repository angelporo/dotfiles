#!/usr/bin/env bash
# 把本仓库的配置软链到 Rime 用户目录，让两边始终同步。
# 用法：bash link.sh        （幂等，可反复执行）
set -euo pipefail

RIME_DIR="${RIME_DIR:-$HOME/Library/Rime}"
CFG="$(cd "$(dirname "$0")" && pwd)/config"

[[ -d "$RIME_DIR" ]] || { echo "✗ 找不到 Rime 用户目录：$RIME_DIR"; exit 1; }

# 软链指定文件（Rime 不会改写它们，所以可以放心软链）
link() {
  local name="$1" dest="$RIME_DIR/$name" target="$CFG/$name"
  [[ -f "$target" ]] || { echo "✗ 缺少 $target"; exit 1; }
  if [[ -e "$dest" && ! -L "$dest" ]]; then
    mv "$dest" "$dest.orig-local"
    echo "  备份原文件 → $name.orig-local"
  fi
  rm -f "$dest"
  ln -s "$target" "$dest"
  echo "  $name → $target"
}

echo "软链配置到 $RIME_DIR"
link wanxiang.custom.yaml
link squirrel.custom.yaml

# installation.yaml 会被 Rime 改写（last_build_time 等），只复制不软链
if [[ ! -f "$RIME_DIR/installation.yaml" ]]; then
  cp "$CFG/installation.yaml" "$RIME_DIR/installation.yaml"
  echo "  installation.yaml 已复制（记得按本机路径改 sync_dir）"
fi

echo
echo "完成。请到输入法菜单点「重新部署」。"
