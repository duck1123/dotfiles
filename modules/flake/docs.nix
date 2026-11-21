# Option reference for this repo, built with nixosOptionsDoc and mdBook.
#
#   nix build .#docs     # site in result/share/doc/dotfiles
#   nur docs             # build and open it
#
# Options come from a real host's evaluation (edgenix), filtered to those
# declared in this repo, so nixpkgs/home-manager options are left out. The
# `host` tree is identical on every host, so one host is enough.
{ inputs, lib, ... }:
let
  inherit (inputs) self;

  docHost = "edgenix";
  nixosOptions = self.nixosConfigurations.${docHost}.options;
  homeOptions = self.homeConfigurations."duck@${docHost}".options;

  repoUrl = "https://github.com/duck1123/dotfiles/blob/master";
  src = "${toString self.outPath}/";

  # "/nix/store/...-source/modules/x.nix, via option flake.modules..." -> "modules/x.nix"
  repoPath =
    declaration:
    let
      path = toString declaration;
    in
    if lib.hasPrefix src path then
      lib.head (lib.splitString ", via option" (lib.removePrefix src path))
    else
      null;

  # Modules from flake inputs that are imported as plain attrsets get the
  # importing file as their declaration, so they'd pass the path test above.
  fromInputs = [
    "programs.zen-browser" # zen-browser homeModule, imported in modules/base.nix
    "programs.lookapp" # lookapp nixosModule, imported in modules/flake/environments.nix
  ];

  fromRepo =
    option:
    lib.any (d: repoPath d != null) option.declarations
    && !lib.elem (lib.concatStringsSep "." (lib.take 2 option.loc)) fromInputs;

  # `hosts` and `identities` are read-only mirrors of the registries; their
  # schema is already documented under `host` and `host.identity`.
  mirrors = [
    "hosts"
    "identities"
  ];

  mkDoc =
    options: keep: pkgs:
    pkgs.nixosOptionsDoc {
      inherit options;
      warningsAreErrors = false;
      transformOptions =
        option:
        option
        // {
          visible = option.visible && fromRepo option && keep (lib.head option.loc);
          declarations = map (
            d:
            let
              path = repoPath d;
            in
            if path == null then
              d
            else
              {
                name = path;
                url = "${repoUrl}/${path}";
              }
          ) option.declarations;
        };
    };

  pages = [
    {
      file = "host.md";
      title = "Host options";
      intro = ''
        The `host` option every NixOS and home-manager module sees. Hosts set it in
        `hosts/<hostname>.nix`; features appear here as `host.features.<name>`.
      '';
      doc = mkDoc nixosOptions (top: top == "host");
    }
    {
      file = "nixos.md";
      title = "NixOS options";
      intro = "Other NixOS options declared in this repo.";
      doc = mkDoc nixosOptions (top: top != "host" && !lib.elem top mirrors);
    }
    {
      file = "home-manager.md";
      title = "Home Manager options";
      intro = "Other home-manager options declared in this repo.";
      doc = mkDoc homeOptions (top: top != "host" && !lib.elem top mirrors);
    }
  ];

  bookToml = ''
    [book]
    title = "Dotfiles options"
    src = "src"

    [output.html]
    no-section-label = true

    [output.html.search]
    limit-results = 50
  '';

  summary = ''
    # Summary

    [Introduction](README.md)

    ${lib.concatMapStrings (page: "- [${page.title}](${page.file})\n") pages}
  '';

  readme = ''
    # Dotfiles options

    Generated from the module declarations in this repo by `nixosOptionsDoc`,
    using the `${docHost}` configuration. Defaults are shown as that
    evaluation computes them. Rebuild with `nix build .#docs` or `nur docs`.
  '';
in
{
  perSystem =
    { pkgs, system, ... }:
    lib.optionalAttrs (system == "x86_64-linux") {
      packages.docs = pkgs.runCommand "dotfiles-docs" { nativeBuildInputs = [ pkgs.mdbook ]; } ''
        mkdir -p book/src
        cp ${pkgs.writeText "book.toml" bookToml} book/book.toml
        cp ${pkgs.writeText "SUMMARY.md" summary} book/src/SUMMARY.md
        cp ${pkgs.writeText "README.md" readme} book/src/README.md
        ${lib.concatMapStrings (page: ''
          {
            printf '# %s\n\n%s\n' ${lib.escapeShellArg page.title} ${lib.escapeShellArg page.intro}
            cat ${(page.doc pkgs).optionsCommonMark}
          } > book/src/${page.file}
        '') pages}
        mdbook build book --dest-dir $out/share/doc/dotfiles
      '';
    };
}
