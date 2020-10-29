import dracula.draw

# Load existing settings made via :set
config.load_autoconfig()

# set theme
dracula.draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})

# autosave
c.auto_save.session = True

# tabs
c.colors.tabs.selected.even.bg = "#5d478b"
c.colors.tabs.selected.odd.bg = "#5d478b"
c.colors.tabs.pinned.selected.even.bg = "#5d478b"
c.colors.tabs.pinned.selected.odd.bg = "#5d478b"

c.tabs.padding = {'top': 0, 'bottom': 0, 'left': 0, 'right': 0}

# status bar
c.statusbar.padding = {'top': 0, 'bottom': 0, 'left': 0, 'right': 0}

# webpage
# requires Qt1.5..
# c.colors.webpage.prefers_color_scheme_dark = True
#c.qt.args = ["blink-settings=darkMode=4"]

# keybindings
config.bind('<Ctrl-g>', 'leave-mode')
