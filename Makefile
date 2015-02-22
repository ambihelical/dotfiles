
LN = scripts/safe_link
CFG = ~/.config
CACHE=~/.cache

BASH_FILES = ~/.bashrc ~/.bash_profile ~/.inputrc
DIR_FILES = ~/bin ~/local
APP_FILES = ~/.screenrc ${CFG}/ack/config
SCM_FILES = ${CFG}/git/config  ${CFG}/git/ignore
WM_FILES = ~/.xsession ~/.Xmodmap ${CFG}/i3/config ${CFG}/i3status/config ${CFG}/i3/status.py
VIM_FILES = ~/.vimrc ${CFG}/vim  ${CACHE}/vim
BIN_FILES=$(foreach bin,$(notdir $(wildcard ${PWD}/bin/*)),~/bin/${bin})


.PHONY: help base dev i3 all defaults

help:
	@echo "The following targets can be used"
	@echo "   help     - what you are seeing now"
	@echo "   base     - bash, directories, etc"
	@echo "   dev      - git, screen, vim"
	@echo "   defaults - override system defaults"
	@echo "   i3       - i3 configuration"
	@echo "   all      - all of the above"

base: ${BASH_FILES} ${DIR_FILES} ${BIN_FILES}
	@echo "base configured"

dev: ${VIM_FILES} ${APP_FILES} ${SCM_FILES}
	@echo "dev configured"

i3: ${WM_FILES}
	@echo "i3 configured"


# fix some annoying default settings
defaults:
	xdg-mime default pcmanfm.desktop inode/directory
	xdg-mime default chromium.desktop x-scheme-handler/http
	xdg-mime default chromium.desktop x-scheme-handler/https
	xdg-mime default chromium.desktop text/html
	xdg-mime default gvim.desktop `grep '^text/*' /usr/share/mime/types`

all: base dev i3 defaults

~/.bash_profile: ${PWD}/bashrc
	${LN} $< $@

~/local: ~/.local
	${LN} $< $@

~/bin:
	mkdir -p ~/bin

${CFG}/vim: ${PWD}/vimdir
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

${CACHE}/%:
	mkdir -p $@
