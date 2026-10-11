{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    let
      # claude-code is unfree, which perSystem's default `pkgs` refuses.
      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
      # nix build .#trilium-claude-code
      # Mounted at /nix inside the trilium container (appTemplates/trilium.nix)
      # for its "Claude Code" AI provider. A real flake package resolved via
      # the storePath convention rather than a nixExpr string: unfree packages
      # aren't on cache.nixos.org, so a nixExpr made nix-csi fetch nixpkgs and
      # build the whole thing in-cluster on every node, which ran past its
      # 300s build timeout. Built here instead, it's substituted from Attic.
      packages.trilium-claude-code = pkgs.claude-code;
    };
}
