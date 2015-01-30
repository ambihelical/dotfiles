
LN = scripts/safe_link
BASH_FILES = ~/.bashrc ~/.bash_profile ~/.inputrc
DIR_FILES = ~/bin ~/local
APP_FILES = ~/.screenrc 
SCM_FILES = ~/.gitconfig  ~/.gitignore
WM_FILES = ~/.xsession ~/.Xmodmap
VIM_FILES = ~/.vimrc

.PHONY: help base dev i3 all

help:
	@echo "The following targets can be used"
	@echo "   help - what you are seeing now"
	@echo "   base - bash, directories, etc"
	@echo "   dev  - git, screen, vim"
	@echo "   i3   - i3 configuration"
	@echo "   all  - all of the above"

base: ${BASH_FILES} ${DIR_FILES} 
	@echo "base configured"

dev: ${VIM_FILES} ${APP_FILES} ${SCM_FILES}
	@echo "dev configured"

i3: ${WM_FILES}
	@echo "i3 configured"

all: base dev i3

~/.bashrc: ${PWD}/bashrc
	${LN} $< $@

~/.bash_profile: ${PWD}/bashrc
	${LN} $< $@

~/.inputrc: ${PWD}/inputrc
	${LN} $< $@

~/.Xmodmap: ${PWD}/xmodmap
	${LN} $< $@

~/.screenrc: ${PWD}/screenrc
	${LN} $< $@

~/local: ~/.local
	${LN} $< $@

~/.gitconfig: ${PWD}/gitconfig
	${LN} $< $@

~/.gitignore: ${PWD}/gitignore
	${LN} $< $@

~/bin: ${PWD}/bin
	${LN} $< $@

~/.vimrc: ${PWD}/vimrc
	${LN} $< $@

~/.xsession: ${PWD}/xsession
	${LN} $< $@


