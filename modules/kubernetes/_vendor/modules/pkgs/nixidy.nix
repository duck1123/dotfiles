{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    {
      packages.nixidy = inputs.nixidy.packages.${system}.default;
    };
}
