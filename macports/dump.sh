#!/usr/bin/env bash
# 从当前机器导出 MacPorts 清单，覆盖本目录下的 ports-requested.txt。
#
#   ./dump.sh            # 导出
#   ./dump.sh --check    # 只对比不写入，看本机与仓库清单有没有差异
#
# 只记录「主动安装」的端口（port installed requested），依赖不写进去 ——
# 恢复时 MacPorts 会自动带入依赖。
set -euo pipefail

PORT="${PORT:-/opt/local/bin/port}"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIST="$DIR/ports-requested.txt"
VARLIST="$DIR/ports-variants.txt"

command -v "$PORT" >/dev/null 2>&1 || { echo "✗ 找不到 port（试过 $PORT）" >&2; exit 1; }

# 1. 主动安装的端口名
new="$(mktemp)"
"$PORT" installed requested </dev/null 2>/dev/null \
  | grep "@" | awk '{print $1}' | sort > "$new"

if [[ "${1:-}" == "--check" ]]; then
  old="$(mktemp)"; grep -vE '^\s*(#|$)' "$LIST" | sort > "$old"
  if diff -u "$old" "$new" >/dev/null; then
    echo "✓ 仓库清单与本机一致（$(wc -l < "$new" | tr -d ' ') 个端口）"
  else
    echo "差异如下（< 仓库有本机无 / > 本机新增）："
    diff -u "$old" "$new" || true
  fi
  rm -f "$old" "$new"
  exit 0
fi

cp "$new" "$LIST"
rm -f "$new"
echo "✓ 已导出 $(grep -cvE '^\s*$' "$LIST") 个端口 → $LIST"

# 2. 检测「非默认变体」：默认变体恢复时会自动带上，只有非默认的才必须记录
#    port 的变体列表里 [+]xxx 表示默认开启
tmpv="$(mktemp)"
"$PORT" installed requested </dev/null 2>/dev/null | grep "@" | grep "+" \
  | awk '{ n=$1; v=$2; i=index(v,"+");
           if (i>0) { vs=substr(v,i); gsub(/\+/," ",vs); sub(/^ /,"",vs); print n"\t"vs } }' > /tmp/.pv.$$
while IFS=$'\t' read -r name vars; do
  [[ -n "$name" ]] || continue
  defaults=" $("$PORT" variants "$name" </dev/null 2>/dev/null \
              | grep -oE '^\[\+\][a-z0-9_]+' | sed 's/^\[+\]//' | tr '\n' ' ') "
  extra=""
  for v in $vars; do
    case "$defaults" in *" $v "*) ;; *) extra="${extra:+$extra }+$v";; esac
  done
  [[ -n "$extra" ]] && printf '%s %s\n' "$name" "$extra" >> "$tmpv"
done < /tmp/.pv.$$
rm -f /tmp/.pv.$$

if [[ -s "$tmpv" ]]; then
  mv "$tmpv" "$VARLIST"
  echo
  echo "⚠ 检测到非默认变体，已写入 $VARLIST（restore.sh 会自动合并安装）："
  sed 's/^/    /' "$VARLIST"
else
  rm -f "$tmpv" "$VARLIST"
  echo "✓ 所有端口用的都是默认变体，无需额外记录"
fi
