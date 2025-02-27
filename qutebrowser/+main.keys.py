#———————————————————————————————————————————————————————————————————————————————
# +main.keys

#———————————————————————————————————————————————————————————————————————————————
# unbind

config.unbind("<Escape>", mode='insert')
config.unbind("q")
config.unbind("ad")
config.unbind("[[")
config.unbind("]]")
config.unbind("@")
config.unbind("co")
config.unbind("d")
config.unbind("D")
config.unbind('gC')

#———————————————————————————————————————————————————————————————————————————————
# main

config.bind("!", 'config-source')
config.bind("K", 'tab-prev')
config.bind("J", 'tab-next')
config.bind("a", 'open -t')
config.bind("A", 'open -p')
config.bind("X", 'tab-close -p')
config.bind("x", 'tab-close -n')
config.bind("s", 'cmd-set-text -s :tab-select')
config.bind("[", 'tab-prev')
config.bind("]", 'tab-next')
config.bind("{", 'tab-move -')
config.bind("}", 'tab-move +')
config.bind('gc', 'tab-clone')

#———————————————————————————————————————————————————————————————————————————————
# Escape

config.bind("<Shift-Escape>", 'mode-leave', mode='insert')
config.bind("<Shift-Escape>", 'mode-leave', mode='hint')
config.bind("<Shift-Escape>", 'mode-leave', mode='passthrough')
config.bind("<Shift-Escape>", 'mode-leave', mode='caret')
config.bind("<Shift-Escape>", 'mode-leave', mode='register')

config.bind("<Escape>", 'mode-leave', mode='insert')
config.bind("<Escape>", 'mode-leave', mode='hint')
config.bind("<Escape>", 'mode-leave', mode='passthrough')
config.bind("<Escape>", 'mode-leave', mode='caret')
config.bind("<Escape>", 'mode-leave', mode='register')

#———————————————————————————————————————————————————————————————————————————————
# Command

config.bind("<Meta+[>", 'tab-prev', mode='insert')
config.bind("<Meta+]>", 'tab-next', mode='insert')
config.bind("<Meta+[>", 'tab-prev', mode='normal')
config.bind("<Meta+]>", 'tab-next', mode='normal')

config.bind("<Meta+1>", 'tab-focus 1', mode='insert')
config.bind("<Meta+2>", 'tab-focus 2', mode='insert')
config.bind("<Meta+3>", 'tab-focus 3', mode='insert')
config.bind("<Meta+4>", 'tab-focus 4', mode='insert')
config.bind("<Meta+5>", 'tab-focus 5', mode='insert')
config.bind("<Meta+6>", 'tab-focus 6', mode='insert')
config.bind("<Meta+7>", 'tab-focus 7', mode='insert')
config.bind("<Meta+8>", 'tab-focus 8', mode='insert')
config.bind("<Meta+9>", 'tab-focus 9', mode='insert')
config.bind("<Meta+0>", 'tab-focus -1', mode='insert')

config.bind("<Meta+1>", 'tab-focus 1', mode='normal')
config.bind("<Meta+2>", 'tab-focus 2', mode='normal')
config.bind("<Meta+3>", 'tab-focus 3', mode='normal')
config.bind("<Meta+4>", 'tab-focus 4', mode='normal')
config.bind("<Meta+5>", 'tab-focus 5', mode='normal')
config.bind("<Meta+6>", 'tab-focus 6', mode='normal')
config.bind("<Meta+7>", 'tab-focus 7', mode='normal')
config.bind("<Meta+8>", 'tab-focus 8', mode='normal')
config.bind("<Meta+9>", 'tab-focus 9', mode='normal')
config.bind("<Meta+0>", 'tab-focus -1', mode='normal')

#———————————————————————————————————————————————————————————————————————————————
# t

config.bind('td', 'config-cycle colors.webpage.darkmode.enabled true false')
config.bind('tj', 'config-cycle -p -t -u *://{url:host}/* content.javascript.enabled ;; reload')

#———————————————————————————————————————————————————————————————————————————————
# ,

config.bind(',m', 'spawn mpv {url}')
config.bind(',M', 'hint links spawn mpv {hint-url}')
config.bind(',f', 'spawn open -a firefox {url}')
config.bind(',F', 'hint links spawn open -a firefox {hint-url}')

