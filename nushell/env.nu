# Nushell Environment Config
$env.EDITOR = "emacs"
$env.BROWSER = "firefox"
# $env.MANPAGER = "nvim +Man!"
$env.DOTFILES_DIR = ($env | get --optional DOTFILES_DIR | default $"($env.HOME)/dotfiles")
$env.PROJECT_PATHS = [$"($env.HOME)/projects"]
$env.STANDALONE_PROJECTS = [ $env.DOTFILES_DIR ]
$env.XDG_CONFIG_HOME = $"($env.HOME)/.config"
$env.DIRENV_LOG_FORMAT = ""
