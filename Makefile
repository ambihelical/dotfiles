
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
XORG_FILES = ${CFG}/xsettingsd ~/.Xresources ${CFG}/xkb/symbols/local
APP_FILES = ${CFG}/screen ${CFG}/ack $(CFG)/globalrc $(CFG)/pythonrc ${CFG}/rtags/rdmrc \
				$(CFG)/gconf ${CFG}/ripgrep/config ${CFG}/npm/npmrc
GIT_FILES = ${CFG}/git/config  ${CFG}/git/ignore ${CFG}/tig/config ${DATA}/tig
I3_FILES = ${CFG}/i3/config ${CFG}/i3/i3status.config ${CFG}/dunst/dunstrc \
           ${CFG}/gsimplecal/config ${CFG}/i3/three-pane.json ${CFG}/udiskie/config.yml
VIM_FILES = ${CACHE}/vim ${CFG}/vim/vimrc
EXTRA_FILES = ~/.ssh/id_ed25519.pub ${DATA}/fonts/.configured
BAREX_FILES = ~/.xsession ~/.Xmodmap
EMACS_FILES = ${CFG}/emacs/init.el ${CFG}/emacs/early-init.el ${CFG}/emacs/lisp/extras.el ${CFG}/emacs/etc $(CACHE)/emacs ${CFG}/hunspell
ETC_FILES = ${ETC}/sysctl.d/99-edb-sysctl.conf


.PHONY: help base dev i3 all defaults prep-bash barex root

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
	@echo "   barex      - install .xsession, .Xmodmap"

base: prep-bash ${SHELL_FILES} ${DIR_FILES} ${EXTRA_FILES} ${BIN_FILES} ${XORG_FILES}
	@echo "base configured"

prep-bash:
	echo "Saving original bash files (see orig.xxx)"
	rm -f ~/.bash_history
	rm -f ~/.bash_logout
	if [ -f ~/.profile -a ! -h ~/.profile ]; then mv ~/.profile ~/orig.profile; fi
	if [ -f ~/.bashrc -a ! -h ~/.bashrc ]; then mv ~/.bashrc ~/orig.bashrc; fi
	if [ -f ~/.bash_profile -a ! -h ~/.bash_profile  ]; then mv ~/.bash_profile ~/orig.bash_profile; fi

${DATA}/fonts/.configured:
	mkdir -p ${DATA}/fonts
	cp fonts/* ${DATA}/fonts/
	fc-cache -fr
	touch ${DATA}/fonts/.configured
	@echo "Fonts configured"

~/.ssh/id_ed25519.pub:
	ssh-keygen -t ed25519 -a 100

dev: ${VIM_FILES} ${APP_FILES} ${GIT_FILES} ${EMACS_FILES} ${GIT_SCRIPTS}
	@echo "dev configured"

i3: ${I3_FILES}
	@echo "i3 configured"

barex: ${BAREX_FILES}

root: ${ETC_FILES}
	udevadm control --reload-rules   # for udev rules
	sysctl -w vm.swappiness=10    # we have adequate memory

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
	-gsettings set org.gnome.desktop.media-handling autorun-never true
	-gsettings set org.gnome.desktop.wm.preferences audible-bell false
	-gsettings set org.gnome.desktop.interface text-scaling-factor '1.0'
	-gsettings set org.gnome.desktop.default-applications.terminal exec 'urxvt'
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 3600
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type suspend
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-battery-timeout 1800
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type suspend
	-gsettings set org.gnome.nautilus.compression default-compression-format 'zip'
	#-gsettings set org.gnome.nautilus.icon-view captions ['none', 'none', 'none']
	-gsettings set org.gnome.nautilus.icon-view default-zoom-level 'medium'
	#-gsettings set org.gnome.nautilus.list-view default-column-order ['name', 'size', 'type', 'owner', 'group', 'permissions', 'mime_type', 'where', 'date_modified', 'date_modified_with_time', 'date_accessed', 'date_created', 'recency', 'starred']
	#-gsettings set org.gnome.nautilus.list-view default-visible-columns ['name', 'size', 'date_modified']
	-gsettings set org.gnome.nautilus.list-view default-zoom-level 'small'
	-gsettings set org.gnome.nautilus.list-view use-tree-view true
	-gsettings set org.gnome.nautilus.preferences always-use-location-entry false
	-gsettings set org.gnome.nautilus.preferences click-policy 'double'
	-gsettings set org.gnome.nautilus.preferences default-folder-viewer 'list-view'
	-gsettings set org.gnome.nautilus.preferences default-sort-in-reverse-order false
	-gsettings set org.gnome.nautilus.preferences default-sort-order 'name'
	-gsettings set org.gnome.nautilus.preferences fts-enabled true
	-gsettings set org.gnome.nautilus.preferences install-mime-activation true
	-gsettings set org.gnome.nautilus.preferences migrated-gtk-settings true
	-gsettings set org.gnome.nautilus.preferences mouse-back-button 8
	-gsettings set org.gnome.nautilus.preferences mouse-forward-button 9
	-gsettings set org.gnome.nautilus.preferences mouse-use-extra-buttons true
	-gsettings set org.gnome.nautilus.preferences open-folder-on-dnd-hover true
	-gsettings set org.gnome.nautilus.preferences recursive-search 'local-only'
	-gsettings set org.gnome.nautilus.preferences search-filter-time-type 'last_modified'
	-gsettings set org.gnome.nautilus.preferences search-view 'list-view'
	-gsettings set org.gnome.nautilus.preferences show-create-link true
	-gsettings set org.gnome.nautilus.preferences show-delete-permanently true
	-gsettings set org.gnome.nautilus.preferences show-directory-item-counts 'local-only'
	-gsettings set org.gnome.nautilus.preferences show-hidden-files false
	-gsettings set org.gnome.nautilus.preferences show-image-thumbnails 'local-only'
	#-gsettings set org.gnome.nautilus.preferences thumbnail-limit uint64 50
	#-gsettings set org.gnome.nautilus.window-state initial-size (1920, 923)
	-gsettings set org.gnome.nautilus.window-state maximized false


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

# copy git config
${CFG}/git/config: ${PWD}/git/config
	mkdir -p $(dir $@)
	cp -f $< $@

~/bin/git-% : ${PWD}/git/git-scripts/git-%
	chmod a+x $<
	${LN} $< $@

# files in font directory
${DATA}/fonts/% : ${PWD}/fonts/%
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
