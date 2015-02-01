
LN = scripts/safe_link
CFG = ~/.config

BASH_FILES = ~/.bashrc ~/.bash_profile ~/.inputrc
DIR_FILES = ~/bin ~/local
APP_FILES = ~/.screenrc 
SCM_FILES = ${CFG}/git/config  ${CFG}/git/ignore
WM_FILES = ~/.xsession ~/.Xmodmap ${CFG}/i3/config ${CFG}/i3status/config
VIM_FILES = ~/.vimrc ${CFG}/vim

.PHONY: help base dev i3 all defaults

help:
	@echo "The following targets can be used"
	@echo "   help     - what you are seeing now"
	@echo "   base     - bash, directories, etc"
	@echo "   dev      - git, screen, vim"
	@echo "   defaults - override system defaults"
	@echo "   i3       - i3 configuration"
	@echo "   all      - all of the above"

base: ${BASH_FILES} ${DIR_FILES} 
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

~/.bashrc: ${PWD}/bashrc
	${LN} $< $@

~/.bash_profile: ${PWD}/bashrc
	${LN} $< $@

~/.inputrc: ${PWD}/inputrc
	${LN} $< $@

~/.Xmodmap: ${PWD}/xmodmap
	${LN} $< $@

~/.xsession: ${PWD}/xsession
	${LN} $< $@

${CFG}/i3/config: ${PWD}/i3config
	mkdir -p $(dir $@)
	${LN} $< $@

${CFG}/i3status/config: ${PWD}/i3status
	mkdir -p $(dir $@)
	${LN} $< $@

~/.screenrc: ${PWD}/screenrc
	${LN} $< $@

~/local: ~/.local
	${LN} $< $@

${CFG}/git/config: ${PWD}/gitconfig
	mkdir -p $(dir $@)
	${LN} $< $@

${CFG}/git/ignore: ${PWD}/gitignore
	mkdir -p $(dir $@)
	${LN} $< $@

~/bin: ${PWD}/bin
	${LN} $< $@

~/.vimrc: ${PWD}/vimrc
	${LN} $< $@

${CFG}/vim: ${PWD}/vimdir
	${LN} $< $@


