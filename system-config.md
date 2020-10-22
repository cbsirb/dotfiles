# KDE Audio
## Show all your sources at once for easy switching
This adds a new profile with the name which makes it easier to switch between
HDMI and analog sound. I need this because my headphones are connected to my
monitor (when they're not on bluetooth), which is HDMI connected to the laptop.

In `/usr/share/pulseaudio/alsa-mixer/profile-sets/default.conf` (doesn't this
have a local file too? anyway...) add the following:

```
[Profile output:analog-stereo+output:hdmi-stereo-extra1+input:analog-stereo]
description = All (HDMI + Analog)
output-mappings = analog-stereo hdmi-stereo-extra1
input-mappings = analog-stereo
```

Adjust the `analog-stereo` and `hdmi-stereo-extra1` accordingly.

# VPN slice for GlobalProtect
Install https://github.com/dlenski/vpn-slice (nothing special, just follow the
instructions).

Create a script for vpn-slice (easiest way, easy to change). Change the
`10.0.0.0/8` accordingly. Only add here hosts which you really want, we will
change the systemd-resolved to automatically resolve to `*.company.com` over the
vpn dns.

```
#!/usr/bin/env bash

vpn-slice \
hostname1.company.com \
hostname1.company.com \
10.0.0.0/8
```

Now, for the systemd-resolved part. Change the /etc/systemd/resolved.conf to
something along the lines. You could also do this automatically from the
`vpn-config` script using `dns0.tun0` and `dns1.tun0` entries from the
`/etc/hosts` in case the dns addresses are not assigned static.

```
[Resolve]
DNS=10.20.5.16 10.20.6.16
Domains=~lab.company.com ~company.com ~another-subnet.net
```

Restart the `systemd-resolved` service. The part so far is only done once, after
this you only need to connect to the vpn and it will do it's magic.

Use opencconect to connect to the gateway (since it's with `sudo`, `~` will
point to `/root`, so be careful). Change the `vpn.company.com` to an actual
address.

```
sudo openconnect -b --protocol=gp -u $USER vpn.company.com -s '/home/$USER/.local/bin/vpn-config'
# in case it connects but then asks again to connect try this:
sudo openconnect -b --protocol=gp -u $USER https://vpn.company.com/gateway -s '/home/$USER/.local/bin/vpn-config'
```
