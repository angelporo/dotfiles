#!/usr/bin/env bash
# 在全新 Mac 上重建整套万象拼音配置。
#
#   bash install.sh              # 完整安装（含 420MB 语法模型）
#   SKIP_GRAM=1 bash install.sh  # 跳过语法模型（打字更快，整句变弱）
#
# 前置：已装好「鼠须管 Squirrel」输入法。
set -euo pipefail

WX_VERSION="${WX_VERSION:-18.0.15}"
RIME_DIR="${RIME_DIR:-$HOME/Library/Rime}"
HERE="$(cd "$(dirname "$0")" && pwd)"

BASE_URL="https://github.com/amzxyz/rime-wanxiang/releases/download/v${WX_VERSION}/rime-wanxiang-base.zip"
GRAM_URL="https://github.com/amzxyz/RIME-LMDG/releases/download/LTS/wanxiang-lts-zh-hans.gram"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "==> 1/4 下载万象拼音 Base v${WX_VERSION}"
curl -fsSL "$BASE_URL" -o "$tmp/base.zip"
unzip -q "$tmp/base.zip" -d "$tmp/base"

echo "==> 2/4 安装到 $RIME_DIR"
mkdir -p "$RIME_DIR"
cp -R "$tmp/base/." "$RIME_DIR/"

echo "==> 3/4 语法模型（420MB）"
if [[ "${SKIP_GRAM:-0}" == "1" ]]; then
  echo "    已跳过（SKIP_GRAM=1）"
else
  if [[ -f "$RIME_DIR/wanxiang-lts-zh-hans.gram" ]]; then
    echo "    已存在，跳过"
  else
    curl -fsSL "$GRAM_URL" -o "$RIME_DIR/wanxiang-lts-zh-hans.gram"
  fi
fi

echo "==> 4/4 应用个人配置"
bash "$HERE/link.sh"
