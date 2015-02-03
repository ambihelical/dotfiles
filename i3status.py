# -*- coding: utf-8 -*-

import subprocess

from i3pystatus import Status

status = Status(standalone=True)
status.register("clock", format = "%a %-d %b %-I:%M %p",)
status.register("pulseaudio", format="{volume}%",)
status.register("load")
status.register("battery",
    not_present_text="",
    format="{status} {remaining:%E%hh:%Mm}",
    alert=True,
    alert_percentage=5,
    status={
        "DIS":  "Discharging",
        "CHR":  "Charging",
        "FULL": "Bat full",
    },)

status.register("disk", path="/", format="{avail}G on /",)
status.register("network", interface="eth2", format_up="{v4}", unknown_up=True, )
status.register("wireless", interface="wlan0", format_up="{essid} {quality:03.0f}%",)

status.register("pomodoro")

status.run()
