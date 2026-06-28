"""Register a cockpit-gpu plugin in files.js.

Mirrors install.sh's patch_cockpit_files_js.  Appends 'entries' and 'files'
array lines to cockpit's files.js; the plugin's manifest.json fields are
added verbatim.  Skips duplicates so the registration is idempotent.
"""

import re
import sys
from pathlib import Path


def add(text, key, value):
    """Append value to the array identified by key.  Skips if already present."""
    pat = rf'({key}\s*:\s*\[)([^\]]*?)(\n\s*\],)'
    m = re.search(pat, text)
    if not m or value in m.group(2):
        return text
    head, body, tail = m.group(1), m.group(2).rstrip(), m.group(3)
    return text[:m.start()] + head + body + f'\n        {value},' + tail + text[m.end():]


p = Path(sys.argv[1])
jsx_entry, html_entry = sys.argv[2], sys.argv[3]
text = p.read_text()
orig = text

text = add(text, "entries", f'        "{{path": "{jsx_entry}"}}"')
text = add(text, "files",   f'        "{html_entry}"')

if text != orig:
    p.write_text(text)
