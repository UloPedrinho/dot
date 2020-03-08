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

# tabs
c.colors.tabs.selected.even.bg = "#5d478b"
c.colors.tabs.selected.odd.bg = "#5d478b"

c.tabs.padding = {'top': 0, 'bottom': 0, 'left': 0, 'right': 0}

# keybindings
config.bind('<Ctrl-g>', 'leave-mode')
