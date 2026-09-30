#!/usr/bin/env bash
# ==============================================================================
# dotfiles 总入口：把仓库里的配置软链到 home 目录
#
# 设计原则（与 rime/ 一致）：
#   1. 真实配置存放在 ~/dotfiles/<组件>/，git 跟踪的就是它
#   2. home 目录里的 ~/.xxx 只是指向仓库的软链，改一处两边同步
#   3. 每次链接前自动备份原位文件（~/xxx.orig-<时间戳>），可随时回退
#
# 用法：
#   bash ~/dotfiles/setup.sh            # 链接全部组件
#   bash ~/dotfiles/setup.sh shell      # 只链接 shell
#   bash ~/dotfiles/setup.sh git        # 只链接 git
#   bash ~/dotfiles/setup.sh rime       # 调用 rime/link.sh
#   bash ~/dotfiles/setup.sh emacs      # 软链 centaur-emacs 自定义层到 ~/.emacs.d
#   DRY=1 bash ~/dotfiles/setup.sh      # 只打印将要做什么，不真改
#
# 全新机器一键迁移请用 bootstrap.sh（会先装 MacPorts/Squirrel 等前置）。
# ==============================================================================
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOME_DIR="$HOME"
DRY="${DRY:-0}"

# 安全链接：若目标已是指向同一文件的软链则跳过；否则备份原文件后建链
link() {
  local src="$1"   # 仓库里的真实文件（绝对路径）
  local dst="$2"   # home 里的软链位置（绝对路径）
  [ -e "$src" ] || { echo "  ✗ 源不存在: $src"; return 1; }

  # 已经是正确软链
  if [ -L "$dst" ] && [ "$(readlink "$dst")" = "$src" ]; then
    echo "  ✓ 已链接 (跳过): $dst"
    return 0
  fi

  if [ "$DRY" = "1" ]; then
    echo "  → [DRY] link $dst -> $src"
    return 0
  fi

  # 备份真实存在的原文件（不是软链），然后删掉它，让位给软链
  if [ -e "$dst" ] && [ ! -L "$dst" ]; then
    local bak="$dst.orig-$(date +%Y%m%d%H%M%S)"
    cp -a "$dst" "$bak"
    echo "  ⓘ 备份原文件: $bak"
    rm -f "$dst"
  fi

  # 删掉已有的软链或空目录
  [ -L "$dst" ] && rm -f "$dst"
  mkdir -p "$(dirname "$dst")"
  ln -s "$src" "$dst"
  echo "  ✅ 已链接: $dst -> $src"
}

do_shell() {
  echo "==> shell"
  for f in "$HERE"/shell/.*; do
    [ -f "$f" ] || continue
    bn="$(basename "$f")"
    case "$bn" in
      .|..|.DS_Store|README.md) continue ;;
    esac
    link "$f" "$HOME_DIR/$bn"
  done
}

do_git() {
  echo "==> git"
  link "$HERE/git/.gitconfig"        "$HOME_DIR/.gitconfig"
  mkdir -p "$HOME_DIR/.config/git"
  link "$HERE/git/ignore"            "$HOME_DIR/.config/git/ignore"
}

do_rime() {
  echo "==> rime"
  if [ -x "$HERE/rime/link.sh" ]; then
    bash "$HERE/rime/link.sh"
  else
    echo "  ⓘ rime/link.sh 不存在，跳过"
  fi
}

do_emacs() {
  echo "==> centaur-emacs"
  local src="$HERE/centaur-emacs"
  local dst="$HOME_DIR/.emacs.d"
  [ -d "$src" ] || { echo "  ⓘ centaur-emacs/ 不存在，跳过"; return; }
  mkdir -p "$dst"
  for f in "$src"/*; do
    [ -e "$f" ] || continue
    bn="$(basename "$f")"
    case "$bn" in
      .DS_Store|.idea|.git) continue ;;
    esac
    link "$f" "$dst/$bn"
  done
  if [ ! -f "$dst/init.el" ]; then
    echo "  ⚠ ~/.emacs.d/init.el 不存在：centaur 核心尚未安装，Emacs 现在还起不来。"
    echo "    请先装核心：git clone https://github.com/seagle0128/.emacs.d.git \"$dst\""
    echo "    装好核心后，本脚本软链好的自定义文件（custom.el / snippets 等）即可生效。"
  fi
}

main() {
  local target="${1:-all}"
  case "$target" in
    shell) do_shell ;;
    git)   do_git ;;
    rime)  do_rime ;;
    emacs) do_emacs ;;
    all)
      do_shell
      do_git
      do_rime
      do_emacs
      ;;
    *) echo "未知组件: $target（可选 shell / git / rime / emacs / all）"; exit 1 ;;
  esac
  echo
  echo "完成。新开一个终端即可生效；如想回退，删掉软链并把 *.orig-<时间戳> 改名回去。"
}

main "$@"
