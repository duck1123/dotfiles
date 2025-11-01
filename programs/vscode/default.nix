{ config, lib, ... }: {
  config = lib.mkIf config.host.features.vscode.enable {
    programs.vscode = {
      enable = true;
      profiles.default.userSettings = {
        "[nix]"."editor.defaultFormatter" = "brettm12345.nixfmt-vscode";
        "calva.paredit.defaultKeyMap" = "original";
        "diffEditor.hideUnchangedRegions.enabled" = true;
        "direnv.restart.automatic" = true;
        "editor.renderWhitespace" = "trailing";
        "editor.tabSize" = 2;
        "files.autoSave" = "onFocusChange";
        "nix.enableLanguageServer" = true;
        "nix.serverPath" = "nixd";
        "screencastMode.fontSize" = 64.0;
        "telemetry.feedback.enabled" = false;
        "telemetry.telemetryLevel" = "off";
        vs-kubernetes."vs-kubernetes.crd-code-completion" = "enabled";
      };
    };
  };
}
