{ ... }:
{
  flake.modules.generic.identity-duck =
    { ... }:
    {
      identities.duck = {
        name = "Duck Nebuchadnezzar";
        username = "duck";
        email = "duck@kronkltd.net";
        gpgKey = "9564904D297DBF3C";
      };
    };
}
