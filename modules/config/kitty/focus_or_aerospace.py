import subprocess
from kittens.tui.handler import result_handler

CTRL_BYTES = {
    'left':  '\x08',
    'down':  '\x0a',
    'up':    '\x0b',
    'right': '\x0c',
}


def main(args):
    pass


@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    window = boss.window_id_map.get(target_window_id)
    if window is None:
        return
    direction = args[1]
    if 'emacs' in window.title.lower():
        window.write_to_child(CTRL_BYTES[direction])
    else:
        subprocess.run(['/opt/homebrew/bin/aerospace', 'focus', direction])
        subprocess.Popen(['/opt/homebrew/bin/aerospace', 'move-mouse', 'window-lazy-center'])
