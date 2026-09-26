{ lib, ... }:
{
  options.homepageGroups = lib.mkOption {
    description = ''
      Known homepage dashboard groups, in display order. Order here is what
      controls left-to-right/top-to-bottom placement on the dashboard (today
      that just means earlier = appears first under the single-column
      layout; it's also the extension point for per-group column placement
      later). `services.<name>.homepage.group` must be one of these -- add
      new ones here before using them.
    '';
    type = lib.types.listOf lib.types.str;
    default = [ "Apps" ];
  };
}
