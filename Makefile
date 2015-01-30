
LN = scripts/safe_link
BASH_FILES = ~/.bashrc ~/.bash_profile
INPUT_FILES = ~/.Xmodmap ~/.inputrc
APP_FILES = ~/local ~/.screenrc ~/.gitconfig  ~/.gitignore ~/bin

all: ${APP_FILES} ${BASH_FILES} ${INPUT_FILES}

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

