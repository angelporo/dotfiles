#!/usr/bin/env bash
# 在新机器上重建 MacPorts 环境
# 前置条件：先安装 MacPorts 本体 —— https://www.macports.org/install.php
#
# 用法：./restore.sh
#
# 注意：ports-requested.txt 只记录端口名，不包含变体（variant）。
# 如果某些端口你用了非默认变体（例如 ffmpeg +nonfree），请手动追加到清单里，
# 形如：ffmpeg +nonfree

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIST="$SCRIPT_DIR/ports-requested.txt"

if [[ ! -f "$LIST" ]]; then
  echo "错误：找不到清单文件 $LIST" >&2
  exit 1
fi

if ! command -v port >/dev/null 2>&1; then
  echo "错误：未检测到 MacPorts，请先安装 https://www.macports.org/install.php" >&2
  exit 1
fi

COUNT=$(grep -cve '^\s*$' "$LIST")

echo "==> 更新 MacPorts 与 ports 索引"
sudo port selfupdate

echo "==> 安装 $COUNT 个端口（依赖会自动带入，耗时视二进制包覆盖率而定）"
# shellcheck disable=SC2046
sudo port install $(tr '\n' ' ' < "$LIST")

echo "==> 安装后自检：扫描二进制链接错误"
sudo port rev-upgrade

echo "==> 完成。已激活端口总数：$(port installed 2>/dev/null | grep -c '(active)')"
