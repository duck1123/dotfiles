# Import flake-parts modules module
{ inputs, ... }: {
  imports = [ inputs.flake-parts.flakeModules.modules ];
}
