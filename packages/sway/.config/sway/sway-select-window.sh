#!/bin/bash
window_id=$(swaymsg -t get_tree | jq -r '.. | select(.type? == "con" and .name) | .id as $id | "\(.name) (\($id))"' | wofi --dmenu --insensitive --prompt="Select window" | grep -oP '\(\K[^\)]*')
if [[ -n $window_id ]]; then
  swaymsg "[con_id=$window_id]" focus
fi
