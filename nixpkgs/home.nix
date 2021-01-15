{ config, pkgs, ... }:

let

  latex = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-medium
      wrapfig capt-of preview lxfonts latexmk
      natbib biblatex biblatex-abnt fourier xpatch;
  };

  userBase = with pkgs; [
    # audio
    pulsemixer
    # image
    feh sxiv imagemagickBig scrot ffmpeg
    # office
    poppler zathura
    # themes
    gnome3.adwaita-icon-theme bibata-cursors-translucent
    # utils
    bat stow htop neofetch zip unzip gzip unrar bzip2 xz
    dropbox
    # video
    autorandr vulkan-loader
    # libs
    imlib2 libtool
    # xorg
    xclip xorg.xset xorg.xsetroot xorg.xrdb
    xorg.xmodmap xorg.xprop xorg.xrandr xorg.xfontsel
  ];

  devBase = with pkgs; [
    gcc clojure leiningen
    adoptopenjdk-bin
    pandoc gnuplot graphviz
    sqlite ripgrep
    latex diffutils file
  ];

  nixTools = with pkgs; [
    nixpkgs-fmt nix-update rnix-lsp
  ];

in

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz; })
  #   )
  # ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = { username = "rxf4el";
           homeDirectory = "/home/rxf4el";
           stateVersion = "21.03";
           keyboard = { layout = "br";
                        variant = "abnt2";
                        options = ["ctrl:nocaps"];
           };
           packages = userBase ++ devBase ++ nixTools;
         };

  xdg = {
    enable = true;
    mimeApps = {
      associations.added = {
        "x-scheme-handler/org-protocol" = [
          "org-protocol.desktop" ];
      };
    };
  };

  programs = {
    emacs = { enable = true;
              # package = pkgs.emacsGcc;
              # extraPackages = (epkgs: [ epkgs.vterm ] );
            };

    firefox = { enable = true; };

    git = { enable = true;
            userName = "rxf4el";
            userEmail = "rxf4e1@tuta.io";
          };

    fzf = { enable = true;
            enableBashIntegration = true;
            defaultOptions = [ "--color=bg+:24" ];
          };
  };

  # services = { emacs.enable = true; };
}
