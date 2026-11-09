# `nix flake check` tests for the simsnix framework (./_simsnix), run against
# the fake catalog in ./_simsnix/test. The tests run at eval time; the check
# derivation only exists to report them.
_: {
  perSystem =
    { lib, pkgs, ... }:
    let
      t = ./_simsnix/test;
      a = t + "/public-a";
      b = t + "/public-b";
      private = t + "/private";

      run =
        args:
        import ./_simsnix/eval.nix (
          {
            inherit lib;
            sources = [
              a
              b
              private
            ];
            profile = "default";
            raw = true;
          }
          // args
        );

      targets = r: map (f: f.target) r.manifest.files;

      # A profile with just these mods, added on top of the private config
      only =
        mods:
        run {
          profile = "test";
          extraModules = [ { sims4.profiles.test.mods = mods; } ];
        };

      fails = infix: r: lib.any (lib.hasInfix infix) r.failures;

      default = run { };
      widgets = default.config.mods.acme.widgets;

      tests = {
        "default profile passes its checks" = default.failures == [ ];

        "default manifest" =
          targets default == [
            "AcmeCore/acme_core.ts4script"
            "acme_variants/variants.package"
            "acme_widgets/widgets.package"
            "acme_widgets/widgets.ts4script"
            "zed_loose.package"
          ];

        "versions merge across sources" =
          lib.attrNames widgets.versions == [
            "1.0"
            "2.0"
          ];

        "the highest version is selected" = widgets.selectedVersion == "2.0";

        "a later source wins a scalar" = widgets.downloadPage == "https://example.com/b/widgets";

        "fields only one source sets are kept" =
          widgets.stripComponents == 1 && widgets.tags == [ "script" ];

        "source order decides, not the file" =
          (run {
            sources = [
              b
              a
              private
            ];
          }).config.mods.acme.widgets.downloadPage == "https://example.com/a/widgets";

        "private config overrides a source" = default.config.mods.acme.core.folder == "AcmeCore";

        "a pin selects an older version" = lib.elem "a101" (
          map (f: f.sha256)
            (run { extraModules = [ { sims4.mods.acme.widgets.version = lib.mkForce "1.0"; } ]; })
            .manifest.files
        );

        "archive junk is not installed" = !(lib.any (lib.hasSuffix "readme.txt") (targets default));

        "an archive member names its archive" =
          lib.filter (f: f.target == "acme_widgets/widgets.package") default.manifest.files == [
            {
              mod = "acme/widgets";
              version = "2.0";
              target = "acme_widgets/widgets.package";
              sha256 = "b202";
              archive = "b200";
              member = "Widgets/widgets.package";
            }
          ];

        "a plain file names no archive" = lib.all (f: f.archive == null && f.member == null) (
          lib.filter (f: f.target == "zed_loose.package") default.manifest.files
        );

        "a mod's own option selects files" = lib.elem "acme_variants/variants-extras.package" (
          targets (run {
            extraModules = [ { sims4.mods.acme.variants.variant = lib.mkForce "full"; } ];
          })
        );

        "flatten drops a member's path" = lib.elem "acme_widgets/widgets.package" (
          targets (run {
            extraModules = [
              {
                sims4.mods.acme.widgets = {
                  stripComponents = lib.mkForce 0;
                  flatten = true;
                };
              }
            ];
          })
        );

        "flatten drops a plain file's path" = lib.elem "zed_loose/loose.package" (
          targets (run {
            extraModules = [
              {
                sims4.mods.zed.loose = {
                  folder = lib.mkForce "zed_loose";
                  flatten = true;
                  versions = lib.mkForce {
                    "1".files = [
                      {
                        name = "Sub/Dir/loose.package";
                        sha256 = "e200";
                      }
                    ];
                  };
                };
              }
            ];
          })
        );

        "flattening onto one name fails" = fails "installed by more than one file" (run {
          extraModules = [
            {
              sims4.mods.zed.loose = {
                flatten = true;
                versions = lib.mkForce {
                  "1".files = [
                    {
                      name = "A/same.package";
                      sha256 = "e300";
                    }
                    {
                      name = "B/same.package";
                      sha256 = "e301";
                    }
                  ];
                };
              };
            }
          ];
        });

        "base profile minus remove" =
          map (m: m.id) (run { profile = "lite"; }).manifest.mods == [
            "acme/core"
            "acme/variants"
            "acme/widgets"
          ];

        "empty profile installs nothing" = (run { profile = "vanilla"; }).manifest.files == [ ];

        "the manifest references no store paths" =
          !(builtins.hasContext (builtins.toJSON default.manifest));

        "unknown profile fails" = fails "unknown profile nope" (run {
          profile = "nope";
        });

        "unknown mod in a profile fails" = fails "unknown mod acme/missing" (only [ "acme/missing" ]);

        "a missing requirement fails" =
          fails "acme/variants: requires acme/core, which is not enabled"
            (only [
              "acme/variants"
            ]);

        "a conflict fails" = fails "conflicts with zed/loose" (run {
          extraModules = [ { sims4.mods.acme.core.conflicts = [ "zed/loose" ]; } ];
        });

        "scripts too deep fail" = fails "at most one folder below Mods/" (run {
          extraModules = [ { sims4.mods.acme.core.folder = lib.mkForce "Acme/Core"; } ];
        });

        "two mods in one folder fail" = fails "folder acmecore is used by" (run {
          extraModules = [ { sims4.mods.zed.loose.folder = lib.mkForce "acmecore"; } ];
        });

        "a version with no files to install fails" = fails "installs no files" (run {
          extraModules = [ { sims4.mods.zed.loose.include = [ ".*\\.ts4script" ]; } ];
        });

        "the tool's entry point throws on failures" =
          !(builtins.tryEval (
            import ./_simsnix/eval.nix {
              inherit lib;
              sources = [ private ];
              profile = "nope";
            }
          )).success;
      };

      failed = lib.attrNames (lib.filterAttrs (_: ok: !ok) tests);
    in
    {
      checks.sims4 =
        if failed == [ ] then
          pkgs.runCommand "sims4-framework-checks" { } ''
            echo "${toString (lib.length (lib.attrNames tests))} simsnix tests passed" > $out
          ''
        else
          throw "simsnix tests failed:\n${lib.concatMapStringsSep "\n" (n: "- ${n}") failed}";
    };
}
