#!/bin/sh
# Renders cheatsheet.html to PDF with headless Chrome. Run from this directory.
CHROME="${CHROME:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"
"$CHROME" --headless --disable-gpu --no-pdf-header-footer --virtual-time-budget=10000 \
  --print-to-pdf=adas.utils-cheatsheet.pdf "file://$PWD/cheatsheet.html" 2>/dev/null
# README thumbnail: the viewport shows page 1 only
"$CHROME" --headless --disable-gpu --hide-scrollbars --window-size=1056,816 --virtual-time-budget=10000 \
  --screenshot=../man/figures/README-cheatsheet.png "file://$PWD/cheatsheet.html" 2>/dev/null
