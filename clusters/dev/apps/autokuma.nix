{ secrets, ... }:
{
  services.autokuma = {
    kuma = {
      username = (secrets.autokuma or { }).username or "";
      password = (secrets.autokuma or { }).password or "";
    };
  };
}
