{ ... }:
{
  flake.modules.generic.hosts =
    { inputs, ... }:
    {
      imports = with inputs.self.modules.generic; [
        edgenix
        inspernix
        nasnix
        pixel8
        powerspecnix
        steamdeck
        vavirl-pw0bwnq8
        vidcentre
      ];
    };
}
