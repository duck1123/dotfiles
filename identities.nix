{ ... }:
rec {
  deck = {
    inherit (duck) name email gpgKey;
    username = "deck";
  };
  drenfer = {
    inherit (duck) gpgKey;
    name = "Daniel E. Renfer";
    username = "drenfer";
    email = "daniel.renfer@vallen.com";
  };
  duck = {
    name = "Duck Nebuchadnezzar";
    username = "duck";
    email = "duck@kronkltd.net";
    gpgKey = "9564904D297DBF3C";
  };

}
