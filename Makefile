
LN = scripts/safe-link
CFG = ~/.config
CACHE=~/.cache
ETC=/etc

SHELL_FILES = ~/.bashrc ~/.bash_profile ${CFG}/readline ~/.profile
DIR_FILES = ~/bin
BIN_FILES=$(foreach bin,$(notdir $(wildcard ${PWD}/bin/*)),~/bin/${bin})
XORG_FILES = ${CFG}/xsettingsd ~/.Xresources
APP_FILES = ${CFG}/screen ${CFG}/ack $(CFG)/globalrc $(CFG)/pythonrc
GIT_FILES = ${CFG}/git/config  ${CFG}/git/ignore
I3_FILES = ${CFG}/i3/config ${CFG}/i3/i3status.config ${CFG}/dunst/dunstrc \
           ${CFG}/gsimplecal/config ${CFG}/i3/split-layout.json \
           ${CFG}/i3/local-setup ${CFG}/i3/xmodmap
VIM_FILES = ~/.vimrc ${CFG}/vim  ${CACHE}/vim
BAREX_FILES = ~/.xsession ~/.Xmodmap
EMACS_FILES = ~/.emacs.d $(CACHE)/emacs
ETC_FILES = ${ETC}/sysctl.d/99-edb-sysctl.conf


.PHONY: help base dev i3 all defaults

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

base: ${SHELL_FILES} ${DIR_FILES} ${BIN_FILES} ${XORG_FILES}
	@echo "base configured"

dev: ${VIM_FILES} ${APP_FILES} ${GIT_FILES} ${EMACS_FILES}
	@echo "dev configured"

i3: ${I3_FILES}
	@echo "i3 configured"

barex: ${BAREX_FILES}

root: ${ETC_FILES}
	udevadm control --reload-rules   # for udev rules

# fix some annoying default settings
defaults:
	find ~ -maxdepth 1 \( -name Desktop -o -name Music -o -name Pictures -o -name Templates -o -name Videos -o -name Public \) -exec rmdir --ignore-fail-on-non-empty {} \;
	-xdg-mime default gvim.desktop `grep '^text/*' /usr/share/mime/types`
	-xdg-mime default nemo.desktop inode/directory application/x-gnome-saved-search
	-xdg-mime default firefox.desktop x-scheme-handler/http
	-xdg-mime default firefox.desktop x-scheme-handler/https
	-xdg-mime default firefox.desktop text/html
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
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 3600
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type suspend
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-battery-timeout 1800
	-gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type suspend

all: base dev i3 defaults

~/.bash_profile: ${PWD}/bashrc
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

${ETC}/%: ./etc/%
	cp -f $< $@
