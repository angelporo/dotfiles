#!/usr/bin/env bash
# 在新机器上重建 MacPorts 环境
#
# 前置：先装 MacPorts 本体 —— https://www.macports.org/install.php
# 用法：./restore.sh
#
# 清单说明：
#   ports-requested.txt  只记端口名（不含变体），由 ./dump.sh 从本机导出
#   ports-variants.txt   仅当某些端口用了「非默认变体」时才存在，
#                        形如 `ffmpeg +nonfree`，恢复时优先按这份装
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIST="$DIR/ports-requested.txt"
VARLIST="$DIR/ports-variants.txt"

[[ -f "$LIST" ]] || { echo "✗ 找不到清单 $LIST" >&2; exit 1; }
command -v port >/dev/null 2>&1 || {
  echo "✗ 未检测到 MacPorts：https://www.macports.org/install.php" >&2; exit 1; }

# 合并两份清单：带变体的条目优先（awk 去重保留首次出现）
specs=()
while IFS= read -r line; do
  [[ -n "$line" ]] && specs+=("$line")
done < <(
  { [[ -f "$VARLIST" ]] && grep -vE '^\s*(#|$)' "$VARLIST"
    grep -vE '^\s*(#|$)' "$LIST"; } | awk '!seen[$1]++'
)

echo "==> 更新 MacPorts 与 ports 索引"
sudo port selfupdate

echo "==> 安装 ${#specs[@]} 个端口（依赖自动带入，耗时视二进制包覆盖率而定）"
for s in "${specs[@]}"; do
  echo "    $s"
done
sudo port install "${specs[@]}"

echo "==> 自检：扫描二进制链接错误（依赖库升级后常见）"
sudo port rev-upgrade

echo "==> 完成。已激活端口：$(port installed 2>/dev/null | grep -c '(active)')"
echo
echo "后续维护提醒："
echo "  sudo port reclaim      # 清理旧版本（MacPorts 不会自动删，会一直涨）"
echo "  ./dump.sh              # 装了新软件后，重新导出清单提交回仓库"
