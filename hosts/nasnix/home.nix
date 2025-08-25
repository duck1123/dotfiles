{ pkgs, ... }: {
  imports = [ ../../programs/base ];

  home = {
    packages = with pkgs; [ nerdfetch ];
    sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
  };
}
