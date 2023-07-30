Setting up a new machine or VM
------------------------------

Personal notes on setting up a new machine or VM.

* install git if necessary
* clone this repo
* checkout contrib-tests branch
* run `sudo scripts/update-base-packages`
* run `make base`. This will create a ssh key, use good pass phrase
* run `ssh-add`
* verify new term can be created without bash issues, and then exit all old shells
* if want to make dotfile changes,
    * add pub key to github
    * change dotfiles git remote to git@github.com:ambihelical/dotfiles.git
* run `make defaults`
* run `sudo scripts/update-dev-packages`
* run `make dev`
* run `sudo scripts/update-script-packages`
* if i3 wanted,
    * run `make i3`
    * run `i3wm-keyboard-setup` and verify works
    * logout and login as i3
* if any rust packages wanted (especially starship prompt),
    * run `scripts/update-rust-packages`
    * answer rustup question
    * remove bashrc and profile changes this makes
    * verify new term has starship prompt
* if emacs wanted,
    * run `sudo scripts/install-emacs`
    * run emacs so it auto configures
