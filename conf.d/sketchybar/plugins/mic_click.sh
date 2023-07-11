#!/bin/bash

MIC_VOLUME=$(osascript -e 'input volume of (get volume settings)')

if [[ $MIC_VOLUME -eq 0 ]]; then
  osascript -e 'set volume input volume 50'
  sketchybar -m --set mic icon=
elif [[ $MIC_VOLUME -gt 0 ]]; then
  osascript -e 'set volume input volume 0'
  sketchybar -m --set mic icon=
fi
