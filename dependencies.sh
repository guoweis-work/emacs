#! /bin/sh

mkdir ~/git
git clone https://github.com/konrad1977/welcome-dashboard ~/git/konrad1977_welcome-dashboard
git clone https://github.com/JasZhe/window-stool ~/git/JasZhe_window-stool
git clone https://github.com/jdtsmith/eglot-booster ~/git/jdtsmith_eglot-booster

# emacs-lsp-booster
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
git clone https://github.com/blahgeek/emacs-lsp-booster ~/git/blahgeek_emacs-lsp-booster
cd ~/git/blahgeek_emacs-lsp-booster/
cargo build --release
