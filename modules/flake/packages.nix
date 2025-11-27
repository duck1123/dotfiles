{ inputs, ... }:
let
  pkgs = import inputs.nixpkgs {
    config.allowUnfree = true;
    system = "x86_64-linux";
  };

  ci-home = pkgs.symlinkJoin {
    name = "ci-home";
    paths = [
      inputs.self.homeConfigurations."duck@inspernix".activationPackage
      inputs.self.homeConfigurations."duck@nasnix".activationPackage
      inputs.self.homeConfigurations."duck@powerspecnix".activationPackage
      inputs.self.homeConfigurations."deck@steamdeck".activationPackage
      inputs.self.homeConfigurations."drenfer@VAVIRL-PW0BWNQ8".activationPackage
    ];
  };

  ci-os = pkgs.symlinkJoin {
    name = "ci-os";
    paths = [
      inputs.self.nixosConfigurations.inspernix.config.system.build.toplevel
      inputs.self.nixosConfigurations.nasnix.config.system.build.toplevel
      inputs.self.nixosConfigurations.powerspecnix.config.system.build.toplevel
    ];
  };

  ci = pkgs.symlinkJoin {
    name = "ci";
    paths = [ ci-home ci-os ];
  };
in {
  perSystem = { system, ... }:
    if system == "x86_64-linux" then {
      packages = { inherit ci ci-home ci-os; };
    } else
      { };
}
