{
  extraOptions =
    { lib, ... }:
    {
      settings = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
        description = ''
          Host-specific VS Code settings, merged over the shared ones in
          features/vscode.nix (host wins).
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
      shared = {
        "[nix]"."editor.defaultFormatter" = "brettm12345.nixfmt-vscode";
        "[nushell]"."outline.showVariables" = false;
        "calva.paredit.defaultKeyMap" = "original";
        "claudeCode.preferredLocation" = "panel";
        "diffEditor.hideUnchangedRegions.enabled" = true;
        "direnv.restart.automatic" = true;
        "editor.renderWhitespace" = "trailing";
        "editor.tabSize" = 2;
        "files.autoSave" = "onFocusChange";
        "git.confirmSync" = false;
        "nix.enableLanguageServer" = true;
        "nix.serverPath" = "nixd";
        "screencastMode.fontSize" = 64.0;
        "telemetry.feedback.enabled" = false;
        "telemetry.telemetryLevel" = "off";
        "trilium.serverUrl" = "https://trilium.home.kronkltd.net";
        vs-kubernetes."vs-kubernetes.crd-code-completion" = "enabled";
      };

      managed = pkgs.writeText "vscode-settings.json" (
        builtins.toJSON (lib.recursiveUpdate shared config.host.features.vscode.settings)
      );
    in
    {
      # No profiles.default.userSettings: that makes settings.json a read-only
      # store symlink. Instead the managed keys are merged into a writable file
      # below, so settings changed in the UI stick unless Nix also sets them.
      programs.vscode.enable = true;

      # What Nix manages, for `nur vscode drift` to compare settings.json against.
      xdg.configFile."dotfiles/vscode-settings.json".source = managed;

      # After linkGeneration, which removes the old settings.json symlink.
      home.activation.vscodeSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
        target=${lib.escapeShellArg "${config.xdg.configHome}/Code/User/settings.json"}
        run mkdir -p "$(dirname "$target")"
        if [ -L "$target" ]; then run rm "$target"; fi

        tmp=$(mktemp)
        if [ ! -f "$target" ]; then
          cp ${managed} "$tmp"
        elif ! ${lib.getExe pkgs.jq} --tab -s '.[0] * .[1]' "$target" ${managed} > "$tmp"; then
          # jq can't parse JSONC (comments, trailing commas)
          warnEcho "Couldn't merge VS Code settings into $target, leaving it unchanged"
          rm "$tmp"
          tmp=
        fi
        if [ -n "$tmp" ]; then
          run install -m644 "$tmp" "$target"
          rm "$tmp"
        fi
      '';
    };
}
