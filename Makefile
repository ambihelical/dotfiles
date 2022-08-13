
LN = scripts/safe-link
CFG = ~/.config
CACHE=~/.cache
DATA=~/.local/share
ETC=/etc

# gpp processing
# @@ starts meta macro (e.g. @@ifeq)
# @[ starts macro call (e.g. @[HOST]@)
# OS, HOST and KERNEL are defined

HOST := $(shell uname -n)
OS := $(shell uname -o)
KERNEL := $(shell uname -s)
GPP := gpp -DOS=${OS} -DHOST=${HOST} -DKERNEL=${KERNEL} -n -U '@[' ']@' '(' ',' ')' '(' ')' '\#' '' -M '@@' '\n' ' ' ' ' '\n' '(' ')'

SHELL_FILES = ~/.bashrc ~/.bash_profile ${CFG}/readline ~/.profile ${CFG}/starship.toml
DIR_FILES = ~/bin ${DATA}/Notes 
BIN_FILES=$(foreach bin,$(notdir $(wildcard ${PWD}/bin/*)),~/bin/${bin})
GIT_SCRIPTS_CMDS = forest wtf whoami fire undo tarball
GIT_SCRIPTS = $(foreach cmd,${GIT_SCRIPTS_CMDS},~/bin/git-${cmd})
XORG_FILES = ${CFG}/xsettingsd ~/.Xresources ${CFG}/xkb/symbols/local
APP_FILES = ${CFG}/screen ${CFG}/ack $(CFG)/globalrc $(CFG)/pythonrc ${CFG}/rtags/rdmrc \
				$(CFG)/gconf ${CFG}/ripgrep/config
GIT_FILES = ${CFG}/git/config  ${CFG}/git/ignore ${CFG}/tig/config ${DATA}/tig
I3_FILES = ${CFG}/i3/config ${CFG}/i3/i3status.config ${CFG}/dunst/dunstrc \
           ${CFG}/gsimplecal/config ${CFG}/i3/three-pane.json ${CFG}/udiskie/config.yml
VIM_FILES = ~/.vimrc ${CACHE}/vim
BAREX_FILES = ~/.xsession ~/.Xmodmap
EMACS_FILES = ${CFG}/emacs/init.el ${CFG}/emacs/early-init.el ${CFG}/emacs/lisp/extras.el $(CACHE)/emacs ${CFG}/hunspell
ETC_FILES = ${ETC}/sysctl.d/99-edb-sysctl.conf


.PHONY: help base dev i3 all defaults rust

help:
	@echo "The following targets can be used"
	@echo "   base       - bash, directories, utils, etc"
	@echo "   dev        - git, vim, screen, ack"
	@echo "   defaults   - override system defaults"
	@echo "   i3         - i3 configuration"
	@echo "   all        - all of the above"
	@echo "Special:"
	@echo "   help       - what you are seeing now"
	@echo "   root       - sudo needed for these"
	@echo "   rust       - post rust installation setup"
	@echo "   barex      - install .xsession, .Xmodmap"

base: ${SHELL_FILES} ${DIR_FILES} ${BIN_FILES} ${XORG_FILES}
	@echo "base configured"

dev: ${VIM_FILES} ${APP_FILES} ${GIT_FILES} ${EMACS_FILES} ${GIT_SCRIPTS}
	@echo "dev configured"

i3: ${I3_FILES}
	@echo "i3 configured"

barex: ${BAREX_FILES}

root: ${ETC_FILES}
	udevadm control --reload-rules   # for udev rules
	sysctl -w vm.swappiness=10    # we have adequate memory

# extra rust installation work, do this after installing rust
# eventually the cargo bit should be done by rustup
rust:
	${LN} ${PWD}/cargo/config.toml ${CARGO_HOME}/config.toml
	mkdir -p ${DATA}/bash-completion/completions
	rustup completions bash > ${DATA}/bash-completion/completions/rustup
	${LN} `rustc --print sysroot`/etc/bash_completion.d/cargo  ${DATA}/bash-completion/completions/cargo

# fix some annoying default settings
defaults:
	find ~ -maxdepth 1 \( -name Desktop -o -name Music -o -name Pictures -o -name Templates -o -name Videos -o -name Public -o -name Documents \) -exec rmdir --ignore-fail-on-non-empty {} \;
	-xdg-mime default gvim.desktop `grep '^text/*' /usr/share/mime/types`
	-xdg-mime default nemo.desktop inode/directory application/x-gnome-saved-search
	-xdg-mime default firefox.desktop x-scheme-handler/http
	-xdg-mime default firefox.desktop x-scheme-handler/https
	-xdg-mime default firefox.desktop text/html
	-gsettings set org.freedesktop.ibus.panel.emoji hotkey "[]"
	-gsettings set org.freedesktop.ibus.panel.emoji unicode-hotkey "[]"
	-gsettings set org.nemo.desktop show-desktop-icons false
	-gsettings set org.nemo.preferences show-full-path-titles true
	-gsettings set org.nemo.preferences start-with-dual-pane true
	-gsettings set org.nemo.preferences enable-delete true
	-gsettings set org.nemo.preferences default-folder-viewer list-view
	-gsettings set org.nemo.preferences show-hidden-files false
	-gsettings set org.nemo.preferences confirm-trash false
	-gsettings set org.nemo.preferences default-sort-order mtime
	-gsettings set org.nemo.preferences default-sort-in-reverse-order true
	-gsettings set org.nemo.preferences ignore-view-metadata true
	-gsettings set org.nemo.preferences desktop-is-home-dir true
	-gsettings set org.nemo.preferences show-image-thumbnails 'never'
	-gsettings set org.nemo.preferences show-advanced-permissions true
	-gsettings set org.nemo.preferences size-prefixes base-2
	-gsettings set org.gnome.desktop.media-handling autorun-never true
	-gsettings set org.gnome.desktop.wm.preferences audible-bell false
	-gsettings set org.gnome.desktop.interface text-scaling-factor '1.0'
	-gsettings set org.gnome.desktop.default-applications.terminal exec 'urxvt'
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 3600
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type suspend
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-battery-timeout 1800
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type suspend

all: base dev i3 defaults

~/.bash_profile: ${PWD}/bashrc
	${LN} $< $@

~/bin:
	mkdir -p ~/bin

${DATA}/tig:
	mkdir -p $@


${CFG}/vim: ${PWD}/vim
	${LN} $< $@

# preprocess xsettingsd
${CFG}/xsettingsd: ${PWD}/xsettingsd
	${GPP} $< > $@

# copy git config and then run private startup
# to customize
${CFG}/git/config: ${PWD}/git/config
	mkdir -p $(dir $@)
	cp -f $< $@
	i3wm-private-startup || true

~/bin/git-% : ${PWD}/git/git-scripts/git-%
	chmod a+x $<
	${LN} $< $@

# files in bin directory
~/bin/% : ${PWD}/bin/%
	${LN} $< $@

# ~/.xxx with simple name map
~/.% : ${PWD}/%
	${LN} $< $@

# ~/.config/app/file
# link from ${PWD}/app/file
${CFG}/%: ${PWD}/%
	mkdir -p $(dir $@)
	${LN} $< $@

# ~/.local/share/dir
# link from ${PWD}/dir
${DATA}/%: ${PWD}/%
	${LN} $< $@

${CACHE}/%:
	mkdir -p $@

$(CFG)/%:
	mkdir -p $@

${ETC}/%: ./etc/%
	cp -f $< $@
