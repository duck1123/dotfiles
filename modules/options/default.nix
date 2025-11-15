{ ... }: {
  flake.modules.generic.options = { inputs, ... }: {
    imports = with inputs.self.modules.generic; [ host-options hosts-options ];
  };
}
