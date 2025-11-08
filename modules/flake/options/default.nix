{ ... }: {
  flake.modules.options.default = { inputs, ... }: {
    imports = with inputs.self.modules.options; [ host hosts identity ];
  };
}
