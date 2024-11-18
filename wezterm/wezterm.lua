local wezterm = require 'wezterm'
local mux = wezterm.mux
local config = wezterm.config_builder()

config.color_scheme = 'Horizon Dark (base16)'
config.font = wezterm.font { family = 'FiraCode Nerd Font Mono' }

config.default_prog = { 'cmd.exe', '/k', '%CMDER_ROOT%\\vendor\\init.bat' }
-- config.hide_tab_bar_if_only_one_tab = true
config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"

config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
    {
        key = '%', mods = 'LEADER|SHIFT', action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    -- Send CTRL-A to the application
    {
        key = 'a',
        mods = 'LEADER|CTRL',
        action = wezterm.action.SendKey { key = 'a', mods = 'CTRL' },
    },
    -- Activate pane using VIM keys.
    {
        key = 'h',
        mods = 'LEADER',
        action = wezterm.action.ActivatePaneDirection 'Left',
    },
    {
        key = 'l',
        mods = 'LEADER',
        action = wezterm.action.ActivatePaneDirection 'Right',
    },
    {
        key = 'k',
        mods = 'LEADER',
        action = wezterm.action.ActivatePaneDirection 'Up',
    },
    {
        key = 'j',
        mods = 'LEADER',
        action = wezterm.action.ActivatePaneDirection 'Down',
    },
    -- Adjust pane size using VIM keys.
    {
        key = 'H',
        mods = 'LEADER',
        action = wezterm.action.AdjustPaneSize { 'Left', 5 },
    },
    {
        key = 'L',
        mods = 'LEADER',
        action = wezterm.action.AdjustPaneSize { 'Right', 5 },
    },
    {
        key = 'K',
        mods = 'LEADER',
        action = wezterm.action.AdjustPaneSize { 'Up', 5 },
    },
    {
        key = 'J',
        mods = 'LEADER',
        action = wezterm.action.AdjustPaneSize { 'Down', 5 },
    },
    -- Copy mode
    {
        key = 'Enter',
        mods = 'LEADER',
        action = wezterm.action.ActivateCopyMode,
    },
    -- Paste
    {
        key = 'V',
        mods = 'CTRL',
        action = wezterm.action.PasteFrom 'Clipboard',
    },
    {
        key = 'p',
        mods = 'LEADER',
        action = wezterm.action.PasteFrom 'PrimarySelection',
    },

    -- Tab management
    {
        key = 'c',
        mods = 'LEADER',
        action = wezterm.action.SpawnTab 'CurrentPaneDomain',
    },
    {
        key = 'p',
        mods = 'LEADER',
        action = wezterm.action.ActivateTabRelative(-1),
    },
    {
        key = 'n',
        mods = 'LEADER',
        action = wezterm.action.ActivateTabRelative(1),
    },
}

wezterm.on('gui-startup', function(cmd)
    local tab, pane, window = mux.spawn_window(cmd or {})
    window:gui_window():maximize()

    pane:split { size = 0.3 }
end)

return config
