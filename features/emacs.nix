{
  description = "Emacs";

  extraOptions =
    { lib, ... }:
    {
      parts = lib.mkOption {
        type = lib.types.attrsOf lib.types.bool;
        default = { };
        example = {
          org-roam = false;
        };
        description = ''
          Per-host overrides for `programs.emacs.parts.<name>.enable`. Parts are
          defined in features/emacs.nix (and by other features, e.g. clojure);
          their elisp lives in resources/emacs/.
        '';
      };
    };

  homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (config.host) features identity;
      el = name: builtins.readFile ../resources/emacs/${name}.el;
    in
    {
      xdg.desktopEntries.emacsclient = {
        name = "Emacs Client";
        genericName = "Text Editor";
        exec = "emacsclient -c %F";
        icon = "emacs";
        mimeType = [
          "text/plain"
          "text/markdown"
          "text/x-markdown"
          "text/org"
          "text/x-org"
        ];
        terminal = false;
        categories = [
          "Utility"
          "TextEditor"
        ];
      };

      xdg.mimeApps.defaultApplications = {
        "text/plain" = [ "emacsclient.desktop" ];
        "text/markdown" = [ "emacsclient.desktop" ];
        "text/x-markdown" = [ "emacsclient.desktop" ];
        "text/org" = [ "emacsclient.desktop" ];
        "text/x-org" = [ "emacsclient.desktop" ];
      };

      programs.ripgrep.enable = true;
      programs.emacs = {
        enable = true;

        # Composed by modules/homeManager/emacs-parts.nix. Orders leave gaps so
        # other modules can slot parts in between.
        parts = lib.mkMerge [
          {
            core = {
              order = 100;
              config = el "core";
              packages =
                epkgs: with epkgs.melpaPackages; [
                  rg
                  base16-theme
                  general
                  nyan-mode
                  paredit
                  rainbow-delimiters
                  smartparens
                  which-key
                  windswap
                ];
            };

            completion = {
              order = 200;
              config = el "completion";
              packages =
                epkgs: with epkgs.melpaPackages; [
                  auto-complete
                  company
                  flycheck
                ];
            };

            languages = {
              order = 300;
              config = el "languages";
              packages =
                epkgs: with epkgs.melpaPackages; [
                  nix-mode
                  nushell-mode
                  nushell-ts-mode
                  prettier
                  reformatter
                  tree-sitter
                  yaml-mode
                ];
            };

            magit = {
              order = 400;
              enable = lib.mkDefault features.git.enable;
              config =
                let
                  args = [ "--verbose" ] ++ lib.optional (identity.gpgKey != null) "--gpg-sign=${identity.gpgKey}";
                in
                ''
                  (use-package magit
                    :ensure t
                    :config
                    (setq magit-commit-arguments '(${lib.concatMapStringsSep " " builtins.toJSON args})))
                '';
              packages = epkgs: [ epkgs.melpaPackages.magit ];
            };

            # Points org at ~/org-roam, so it runs before the org part.
            org-roam = {
              order = 450;
              # Only where the vault is synced.
              enable = lib.mkDefault (features.syncthing.enable && features.syncthing.shares.org-roam.enable);
              config = el "org-roam";
              packages =
                epkgs:
                let
                  melpa = epkgs.melpaPackages;
                  quickroam = epkgs.trivialBuild {
                    pname = "quickroam";
                    version = "git-2025-01-01";
                    src = pkgs.fetchFromGitHub {
                      owner = "meedstrom";
                      repo = "quickroam";
                      rev = "master";
                      sha256 = "sha256-JzhpTZmYHtJ5MBicbP7JCp7b66ahX4piivAIf+8u8u0=";
                    };
                    packageRequires = [
                      melpa.org-roam
                      melpa.pcre2el
                    ];
                  };
                in
                [
                  melpa.org-roam
                  melpa.org-roam-ui
                  quickroam
                ];
            };

            org = {
              order = 500;
              config = el "org";
              packages = epkgs: [ epkgs.melpaPackages.org-ql ];
            };

            lsp = {
              order = 700;
              config = el "lsp";
              packages =
                epkgs: with epkgs.melpaPackages; [
                  lsp-mode
                  lsp-ui
                ];
            };

            projectile = {
              order = 800;
              config = el "projectile";
              packages = epkgs: [ epkgs.melpaPackages.projectile ];
            };

            helm = {
              order = 900;
              config = el "helm";
              packages = epkgs: [ epkgs.melpaPackages.helm-projectile ];
            };
          }

          # Host overrides from `features.emacs.parts`
          (lib.mapAttrs (_: enable: { inherit enable; }) features.emacs.parts)
        ];
      };
    };
}
