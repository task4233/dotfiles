# update gpg key

if [ -e $HOME/.emacs.d/elpa ]; then
    gpg --homedir $HOME/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
else
    echo "replace dotfiles(.*) on home directory"
fi
