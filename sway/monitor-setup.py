#!/usr/bin/env python

import sys
import json
import subprocess


DISPLAY_CONFIGS = [
    [
        {"name": "eDP-1", "cmd": "output {} disable"},
        {"serial": "7MT0181G12DU", "cmd": "output {} mode 1920x1200 position 1920 0 subpixel rgb"},
        {"serial": "7MT0181G290U", "cmd": "output {} mode 1920x1200 position 0 0 subpixel rgb"},
    ]
]


def config_match(sway_displays, disp_config):
    if len(disp_config) != len(sway_displays):
        return False

    for conf in disp_config:
        for display in sway_displays:
            if conf.get("name") == display["name"]:
                break
            elif conf.get("serial") == display["serial"]:
                conf["name"] = display["name"]
                break
        else:
            return False

    return True


def main():
    p = subprocess.run(["swaymsg", "-r", "-t", "get_outputs"], capture_output=True, check=True)
    sway_displays = json.loads(p.stdout)

    for disp_config in DISPLAY_CONFIGS:
        if not config_match(sway_displays, disp_config):
            continue

        print(f"Matched config {disp_config}")

        for display in disp_config:
            subprocess.run(["swaymsg", display["cmd"].format(display["name"])], check=True)
        break
    else:
        print("No conifguration matched, leaving unchanged...")

    return 0


if __name__ == "__main__":
    sys.exit(main())
