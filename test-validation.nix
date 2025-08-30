# Test script to verify host validation
let
  # Import the validation module
  hostValidator = import ./modules/flakeModules/hostValidator.nix { 
    lib = import <nixpkgs/lib>; 
  };
  
  # Test host configurations
  testHosts = {
    validHost = {
      name = "test-host";
      system = "x86_64-linux";
      id = "test-id";
      identity = { name = "Test User"; email = "test@example.com"; gpgKey = "12345678"; };
      hostname = "testhost";
      features = {
        hyprland.enable = true;
        waybar.enable = true;  # This should pass validation
        gnome.enable = false;
        syncthing = {
          enable = true;
          shares = {
            keepass.enable = true;  # This should pass validation
            camera.enable = false;
            org-roam.enable = false;
            renpy.enable = false;
          };
        };
        kubernetes = {
          client.enable = true;
          server.enable = false;
        };
        developer.enable = false;
        java.enable = true;
        backups.enable = false;
        bitcoin.enable = false;
        clojure.enable = false;
        dbt.enable = false;
        dconf.enable = false;
        dunst.enable = false;
        emacs.enable = false;
        emacs-prelude.enable = false;
        email.enable = false;
        flipper.enable = false;
        gaming.enable = false;
        git.enable = false;
        hyprpanel.enable = false;
        i3.enable = false;
        jujutsu.enable = false;
        media.enable = false;
        music.enable = false;
        ncmpcpp.enable = false;
        nfs.enable = false;
        nostr.enable = false;
        nushell.enable = false;
        office.enable = false;
        pictures.enable = false;
        radio.enable = false;
        stylix.enable = false;
        vim.enable = false;
        virtualization.enable = false;
        vscode.enable = false;
        zsh.enable = false;
      };
      nixos = {
        enable = false;
        budgie.enable = false;
        gnome.enable = false;
        hyprland.enable = false;
        i3.enable = false;
        plasma6.enable = false;
      };
      home-manager = {
        enable = true;
      };
    };
    
    invalidHost = {
      name = "invalid-host";
      system = "x86_64-linux";
      id = "invalid-id";
      identity = { name = "Invalid User"; email = "invalid@example.com"; gpgKey = "87654321"; };
      hostname = "invalidhost";
      features = {
        hyprland.enable = true;
        waybar.enable = false;  # This should fail validation
        gnome.enable = false;
        syncthing = {
          enable = true;
          shares = {
            keepass.enable = false;  # This should fail validation
            camera.enable = false;
            org-roam.enable = false;
            renpy.enable = false;
          };
        };
        kubernetes = {
          client.enable = false;
          server.enable = true;  # This should fail validation
        };
        developer.enable = false;
        java.enable = true;  # This should generate a warning
        backups.enable = false;
        bitcoin.enable = false;
        clojure.enable = false;
        dbt.enable = false;
        dconf.enable = false;
        dunst.enable = false;
        emacs.enable = false;
        emacs-prelude.enable = false;
        email.enable = false;
        flipper.enable = false;
        gaming.enable = false;
        git.enable = false;
        hyprpanel.enable = false;
        i3.enable = false;
        jujutsu.enable = false;
        media.enable = false;
        music.enable = false;
        ncmpcpp.enable = false;
        nfs.enable = false;
        nostr.enable = false;
        nushell.enable = false;
        office.enable = false;
        pictures.enable = false;
        radio.enable = false;
        stylix.enable = false;
        vim.enable = false;
        virtualization.enable = false;
        vscode.enable = false;
        zsh.enable = false;
      };
      nixos = {
        enable = false;
        budgie.enable = false;
        gnome.enable = false;
        hyprland.enable = false;
        i3.enable = false;
        plasma6.enable = false;
      };
      home-manager = {
        enable = true;
      };
    };
  };
  
  # Test validation
  testValidation = hostValidator.validateHostsComprehensive testHosts;
  
  # Test individual host validation
  testValidHost = hostValidator.validateHostConfig testHosts.validHost;
  
  # Test warnings generation
  testWarnings = hostValidator.generateAllWarnings testHosts;
  
in {
  inherit testHosts testValidation testValidHost testWarnings;
  
  # Show validation results
  validationResults = {
    validHost = testValidHost;
    allWarnings = testWarnings;
    comprehensiveValidation = testValidation;
  };
}
