import gi
gi.require_version('Atspi', '2.0')
from gi.repository import Atspi

def walk(node, depth, limit=4):
    try:
        role, name = node.get_role_name(), node.get_name()
    except Exception as e:
        print('  ' * depth + f'<error {e}>'); return
    print('  ' * depth + f'{role}: {name!r}')
    if depth >= limit: return
    for i in range(node.get_child_count()):
        child = node.get_child_at_index(i)
        if child is not None: walk(child, depth + 1, limit)

desktop = Atspi.get_desktop(0)
print('applications on the AT-SPI bus:', desktop.get_child_count())
for i in range(desktop.get_child_count()):
    app = desktop.get_child_at_index(i)
    print('APP', app.get_name() if app else None)
    if app and ('java' in (app.get_name() or '').lower() or 'A11y' in (app.get_name() or '') or 'Probe' in (app.get_name() or '')):
        walk(app, 1)
