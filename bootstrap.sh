#!/usr/bin/env bash
# ==============================================================================
# 裸 Mac 一键引导：从零把本仓库 dotfiles 完整迁过去
#
# 顺序（依赖关系决定）：
#   1. Xcode 命令行工具        —— MacPorts 前置
#   2. MacPorts 本体           —— 按 macOS 大版本下载对应 pkg 安装
#   3. port selfupdate + 端口清单 —— macports/restore.sh（38 个包，含 emacs）
#   4. Squirrel 输入法         —— 非 port，从 GitHub 下 .app 装到 /Library/Input Methods
#   5. Rime 个人配置          —— rime/apply.sh（前提：官方 rime-ice 已先装到 ~/Library/Rime，含语法模型）
#   6. dotfiles 软链           —— setup.sh all（shell/git/rime/centaur-emacs）
#   7. 备份 /etc/hosts         —— 仅备份 + 打印差异，不覆盖
#   8. 屏蔽 ⌃⌘空格 表情弹框     —— 系统级，注销后生效
#
# 用法：
#   bash ~/dotfiles/bootstrap.sh            # 跑全流程
#   bash ~/dotfiles/bootstrap.sh macports   # 只装 MacPorts 本体
#   bash ~/dotfiles/bootstrap.sh ports      # 只装端口清单
#   DRY=1 bash ~/dotfiles/bootstrap.sh      # 只打印将做什么，不真改
#
# 说明：
#   - 第 2/3/4/7 步用到 sudo，会要求输入密码
#   - 装端口（第 3 步）耗时取决于二进制包覆盖率，可能十几分钟
#   - Squirrel 装完需到「系统设置→键盘→输入法」手动添加「鼠须管」
# ==============================================================================
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export PATH="/opt/local/bin:$PATH"
DRY="${DRY:-0}"

info() { echo "==> $*"; }
warn() { echo "  ⚠ $*"; }
die()  { echo "✗ $*" >&2; exit 1; }
needs(){ command -v "$1" >/dev/null 2>&1; }

# 每个步骤开头：DRY 模式只打印意图
dry_skip() {
  if [ "$DRY" = 1 ]; then info "[DRY] 将执行步骤：$1"; return 0; fi
  return 1
}

# -----------------------------------------------------------------------------
step_clt() {
  dry_skip "安装 Xcode 命令行工具" && return
  if xcode-select -p >/dev/null 2>&1; then
    info "Xcode 命令行工具已装，跳过"
    return
  fi
  info "安装 Xcode 命令行工具（会弹出系统授权窗口，装完自动继续）"
  xcode-select --install || true
  until xcode-select -p >/dev/null 2>&1; do sleep 5; done
  info "Xcode 命令行工具就绪"
}

# -----------------------------------------------------------------------------
step_macports() {
  dry_skip "安装 MacPorts 本体" && return
  if needs port; then
    info "MacPorts 已装（$(port version 2>/dev/null | head -1)），跳过"
    return
  fi
  info "获取 MacPorts 最新版本号"
  MP_VER="$(curl -fsSL https://raw.githubusercontent.com/macports/macports-base/master/config/macports_version 2>/dev/null || true)"
  [ -n "$MP_VER" ] || die "无法获取 MacPorts 版本号（网络问题？）"

  MAJOR="$(sw_vers -productVersion | cut -d. -f1)"
  info "当前 macOS 大版本：$MAJOR，目标 MacPorts $MP_VER"

  # 优先从 GitHub release 资产里挑对应 macOS 大版本的 pkg
  API="https://api.github.com/repos/macports/macports-base/releases/tags/v${MP_VER}"
  PKG_URL="$(curl -fsSL "$API" 2>/dev/null | grep -oE "https://[^\"]*MacPorts-[^\"]*-${MAJOR}\.pkg" | head -1 || true)"
  [ -n "$PKG_URL" ] || PKG_URL="https://github.com/macports/macports-base/releases/download/v${MP_VER}/MacPorts-${MP_VER}-${MAJOR}.pkg"

  tmp="$(mktemp -d)"
  info "下载 MacPorts 安装包：$PKG_URL"
  if ! curl -fL "$PKG_URL" -o "$tmp/mp.pkg"; then
    rm -rf "$tmp"
    die "下载 MacPorts 失败。可能该 macOS 版本暂未发布对应 pkg，请手动安装：https://www.macports.org/install.php"
  fi
  info "安装 MacPorts（需 sudo）"
  sudo installer -pkg "$tmp/mp.pkg" -target /
  rm -rf "$tmp"
  info "MacPorts 安装完成：$(port version 2>/dev/null | head -1)"
}

# -----------------------------------------------------------------------------
step_ports() {
  dry_skip "更新 MacPorts 并安装端口清单" && return
  needs port || die "MacPorts 未安装，先跑 macports 步骤"
  info "更新 MacPorts 索引并安装 38 个端口（含 emacs，可能耗时）"
  sudo /opt/local/bin/port selfupdate
  bash "$HERE/macports/restore.sh"
}

# -----------------------------------------------------------------------------
step_squirrel() {
  dry_skip "安装 Squirrel 输入法" && return
  if [ -e "/Library/Input Methods/Squirrel.app" ]; then
    info "Squirrel 已装，跳过"
    return
  fi
  info "获取 Squirrel 最新发布"
  API="https://api.github.com/repos/rime/squirrel/releases/latest"
  ZIP_URL="$(curl -fsSL "$API" 2>/dev/null | grep -oE "https://[^\"]*\.zip" | head -1 || true)"
  [ -n "$ZIP_URL" ] || die "找不到 Squirrel 发布包，请手动下载：https://github.com/rime/squirrel/releases/latest"

  tmp="$(mktemp -d)"
  info "下载并解压 Squirrel：$ZIP_URL"
  curl -fL "$ZIP_URL" -o "$tmp/squirrel.zip"
  unzip -q "$tmp/squirrel.zip" -d "$tmp"

  info "安装 Squirrel 到 /Library/Input Methods（需 sudo）"
  sudo rm -rf "/Library/Input Methods/Squirrel.app"
  sudo cp -R "$tmp"/Squirrel.app "/Library/Input Methods/Squirrel.app"
  sudo chown -R root:wheel "/Library/Input Methods/Squirrel.app"
  rm -rf "$tmp"
  info "Squirrel 已安装。请到「系统设置→键盘→输入法」添加「鼠须管」。"
}

# -----------------------------------------------------------------------------
step_rime() {
  dry_skip "套用 Rime 个人配置（apply.sh）" && return
  info "套用个人 Rime 配置：rime/apply.sh"
  info "ⓘ 前提：官方 rime-ice 需先安装到 ~/Library/Rime（含 420MB 语法模型）。"
  info "   若尚未安装，请先按 rime-ice 官方文档部署，再单独跑：bash ~/dotfiles/rime/apply.sh"
  bash "$HERE/rime/apply.sh"
}

# -----------------------------------------------------------------------------
step_setup() {
  dry_skip "软链 dotfiles 配置（shell/git/rime/centaur-emacs）" && return
  info "执行 setup.sh all"
  bash "$HERE/setup.sh" all
}

# -----------------------------------------------------------------------------
step_hosts() {
  dry_skip "备份 /etc/hosts 并打印差异（不覆盖）" && return
  info "备份 /etc/hosts（不覆盖，覆盖有风险）"
  BAK="/etc/hosts.bak-$(date +%Y%m%d%H%M%S)"
  sudo cp /etc/hosts "$BAK"
  info "已备份到 $BAK"
  echo "--- 仓库 hosts 与当前系统 hosts 差异（左仓库 / 右系统）---"
  diff "$HERE/etc/hosts" /etc/hosts || true
  warn "未自动覆盖 /etc/hosts（含一份旧 googlehosts，覆盖会影响系统网络）。"
  warn "确认要应用时手动执行：sudo cp \"$HERE/etc/hosts\" /etc/hosts"
}

# -----------------------------------------------------------------------------
step_emoji() {
  dry_skip "屏蔽 ⌃⌘空格 表情弹框" && return
  info "写入 NSUserKeyEquivalents 屏蔽「表情与符号」（需注销生效）"
  defaults write -g NSUserKeyEquivalents -dict-add "表情与符号" '@~^E'
  info "已设置，注销重登后生效。"
}

# -----------------------------------------------------------------------------
main() {
  local target="${1:-all}"
  case "$target" in
    clt)        step_clt ;;
    macports)   step_macports ;;
    ports)      step_ports ;;
    squirrel)   step_squirrel ;;
    rime)       step_rime ;;
    setup)      step_setup ;;
    hosts)      step_hosts ;;
    emoji)      step_emoji ;;
    all)
      step_clt
      step_macports
      step_ports
      step_squirrel
      step_rime
      step_setup
      step_hosts
      step_emoji
      ;;
    *) die "未知步骤: $target（可选 clt/macports/ports/squirrel/rime/setup/hosts/emoji/all）" ;;
  esac
  echo
  echo "引导完成。建议：注销重登一次，让输入法与 ⌃⌘空格 屏蔽生效。"
}

main "$@"
