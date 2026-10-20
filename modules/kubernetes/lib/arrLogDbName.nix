_: {
  # The *arr apps keep logs in a second database next to their main one:
  # "<main>-log", or "<app>-log" when the main one is "<app>-main".
  flake.lib.arrLogDbName =
    { lib }:
    mainName:
    if lib.hasSuffix "-main" mainName then
      lib.removeSuffix "-main" mainName + "-log"
    else
      "${mainName}-log";
}
