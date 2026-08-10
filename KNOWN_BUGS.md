# Known Bugs & Active Workarounds

This file tracks hardware/driver bugs affecting managed systems and the
workarounds deployed through this dotfiles repo or manual system configuration.

When a bug is marked with a **Check condition**, the dotfiles-expert skill
should verify whether the upstream fix has been released before recommending
removal of the workaround.

---

## NVIDIA-001: RTX 5090 hard freeze after idle on KDE Wayland

- **Status:** Active (no upstream fix)
- **Affected systems:** mimir (RTX 5090 / GB202, KDE Plasma 6 Wayland)
- **Driver:** nvidia 590.48.01 (also reproduced on 575.x, 580.x)
- **Symptoms:** After idling 1+ hours, the system hard-locks on monitor wake.
  kwin_wayland pegs 100% CPU, VT switching fails, reboot hangs. Requires hard
  power-off. Logs show zero errors before the freeze (no Xid, no OOM, no MCE).
- **Upstream reference:** https://forums.developer.nvidia.com/t/graphics-freeze-after-a-long-idle-period-on-an-rtx-5090/345760
- **Date identified:** 2026-02-09

### Workarounds applied

#### 1. DPMS screen blanking disabled

Prevents monitors from entering power-save mode, avoiding the wake path that
triggers the freeze.

- **File:** `~/.config/powermanagementprofilesrc`
  ```ini
  [AC][DPMSControl]
  idleTime=0
  lockBeforeTurnOff=0
  ```
- **Managed by:** kwriteconfig6 (not yet chezmoi-managed)
- **To remove:** Delete the `[AC][DPMSControl]` section or set `idleTime` to a
  non-zero value (e.g., `600` for 10 minutes).

#### 2. GPU clock lock (210-2407 MHz)

Clamps GPU clocks to spec boost (2407 MHz), preventing beyond-spec clock
states (driver reports 3210 MHz max) and reducing aggressive power state
transitions during idle.

- **File:** `/etc/systemd/system/nvidia-clock-lock.service`
  ```
  ExecStart=/usr/bin/nvidia-smi -pm 1
  ExecStart=/usr/bin/nvidia-smi -lgc 210,2407
  ExecStop=/usr/bin/nvidia-smi -rgc
  ```
- **Depends on:** `nvidia-persistenced.service` (also enabled as part of this
  workaround)
- **Managed by:** Manual systemd service (not yet chezmoi-managed)
- **To remove:**
  ```bash
  sudo systemctl disable --now nvidia-clock-lock
  sudo nvidia-smi -rgc
  sudo rm /etc/systemd/system/nvidia-clock-lock.service
  sudo systemctl daemon-reload
  ```

### Check condition

This bug can be considered fixed when **all** of the following are true:
1. NVIDIA releases a driver version **newer than 590.48.01** that explicitly
   mentions a fix for GB202/RTX 5090 idle freeze or DPMS wake lockup.
2. The upstream forum thread is marked resolved or an NVIDIA engineer confirms
   the fix.
3. The system survives multiple overnight idle periods without freezing after
   removing both workarounds.

To check for a newer driver:
```bash
pacman -Si nvidia-utils | grep Version
```

---

## USB-001: GenesysLogic USB hub chain resets disconnect all downstream devices

- **Status:** Active (hardware/firmware limitation)
- **Affected systems:** mimir
- **Symptoms:** A chain of two GenesysLogic USB 2.1 Hubs (05e3:0610) on Bus 001
  Port 3 repeatedly disconnects and reconnects, taking all downstream devices
  with it. The Logitech PowerPlay Wireless Charging System (046d:c53a) is at the
  end of the chain, providing the G502 X PLUS mouse and "Candy" keyboard via its
  built-in receiver. Each hub reset cascades down the chain, causing full
  re-enumeration of both hubs and the Logitech receiver. Device numbers climb
  rapidly (50+ re-enumerations per boot).
- **Topology:**
  ```
  Bus 001 Port 3: GenesysLogic Hub (external hub)
  └── Port 4: GenesysLogic Hub (internal to PowerPlay mat)
      └── Port 4: Logitech PowerPlay Wireless Charging System
                   └── G502 X PLUS mouse + Candy keyboard
  ```
- **Date identified:** 2026-02-05

### Workaround applied

- **File:** `/etc/udev/rules.d/90-usb-fixes.rules` (disables autosuspend on
  GenesysLogic hubs)
- **Managed by:** `run_once_before_fix-usb-mimir.sh.tmpl`
- Reduces frequency of resets but does not fully eliminate them.

### Potential fixes

1. **Connect the PowerPlay mat directly to a motherboard USB port** with a
   longer cable, bypassing the external hub entirely. This eliminates the
   cascading hub chain and isolates whether the external hub is the root cause.
2. **Replace the external USB hub** with a non-GenesysLogic hub that handles
   autosuspend/resume more reliably.

### Check condition

This bug can be considered resolved when the PowerPlay mat maintains a stable
connection without repeated re-enumerations. If a direct motherboard connection
is stable, the udev autosuspend rule can be removed and the external hub should
be replaced or retired. If disconnects persist even on a direct connection, the
issue is in the PowerPlay mat's internal hub and Logitech should be contacted.

---

## USB-002: USB 3.0 port 2 constant enumeration spam

- **Status:** Active (hardware issue)
- **Affected systems:** mimir
- **Symptoms:** `usb usb2-port2: Cannot enable. Maybe the USB cable is bad?`
  logged every 4 seconds continuously. No USB 3.0 device is connected to this
  port; the USB 2.0 side works fine.
- **Date identified:** 2026-02-05

### Workaround applied

- **File:** `/etc/systemd/system/disable-usb2-port2.service` (disables the port)
- **Managed by:** `run_once_before_fix-usb-mimir.sh.tmpl`

### Check condition

This may be a physical defect on the motherboard's USB 3.0 port. Test with a
known-good USB 3.0 device directly connected. If it still fails, the port is
likely defective and the workaround should remain permanent. A BIOS update
could also resolve it if the issue is in the USB controller firmware.

---

## BT-001: MT7927 Bluetooth firmware missing from linux-firmware

- **Status:** Active (upstream firmware not redistributable yet)
- **Affected systems:** mimir
- **Symptoms:** `bluetooth hci0: Direct firmware load for
  mediatek/mt7927/BT_RAM_CODE_MT6639_2_1_hdr.bin failed with error -2` repeating
  roughly 100× per minute — 6,642 entries in a single boot, about 150k/day.
  `bluetoothctl show` reports "No default controller available".
- **Upstream reference:**
  https://gitlab.com/kernel-firmware/linux-firmware/-/merge_requests/946
- **Date identified:** 2026-08-09

### Cause

mimir's WiFi/BT combo card is a MediaTek MT7927 (Filogic 380, PCI `14c3:7927`;
the Bluetooth half enumerates as Foxconn `0489:e13a`). The `btmtk` driver
declares `mediatek/mt7927/BT_RAM_CODE_MT6639_2_1_hdr.bin` as required firmware,
but the blob is **not shipped in linux-firmware**. It sits in draft merge
request !946 awaiting MediaTek's redistribution sign-off. The
`mediatek/mt7927/` directory ships the WiFi firmware only.

Not caused by the 2026-08-09 upgrade — the blob has never been in
linux-firmware. Whether it was equally noisy beforehand could not be confirmed,
as no pre-reboot journal survived.

Disabling `bluetooth.service` does **not** stop this. The retry loop is in the
kernel driver, not `bluetoothd` — measured at 102 failures/60s with the service
stopped and inactive. Only unloading/blacklisting `btusb` ends it.

### Workaround applied

- **File:** `/etc/modprobe.d/blacklist-mt7927-bluetooth.conf` (`blacklist btusb`)
- **Managed by:** `run_onchange_before_disable-bluetooth-mimir.sh.tmpl`
- Also disables `bluetooth.service`, which has nothing to manage once `btusb`
  is blacklisted.
- Verified with `modprobe -n -v -b btusb` that udev will not auto-load it at
  boot; failure rate confirmed at 0.

Safe here because mimir uses neither Bluetooth nor WiFi — it is ethernet-only
(`enp12s0`), and the mouse and keyboard run on a Logitech PowerPlay USB
receiver. The MT7927 WiFi side does not present a network interface either, so
the card appears to be entirely non-functional on Linux (untested and unneeded
here).

### Check condition

Remove the blacklist once MR !946 merges and `linux-firmware-mediatek` ships
`BT_RAM_CODE_MT6639_2_1_hdr.bin`. To reverse early (e.g., if a USB Bluetooth
dongle is added):

```bash
sudo rm /etc/modprobe.d/blacklist-mt7927-bluetooth.conf
sudo systemctl enable --now bluetooth
sudo modprobe btusb
```

The firmware can also be extracted manually from ASUS's Windows driver package
(it lives inside the `mtkwlan.dat` container), but that was not pursued since
the hardware is unused.

---

## NET-001: SSDP discovery replies flood the kernel log via UFW

- **Status:** Worked around (logging suppressed; upstream behaviour unchanged)
- **Affected systems:** mimir
- **Symptoms:** `[UFW BLOCK] ... SRC=192.168.1.63 ... SPT=1900 DPT=<ephemeral>`
  logged in bursts every two minutes. 1,206 of 1,315 lines in the kernel ring
  buffer were UFW entries — 92% noise, leaving only 109 real kernel messages
  and roughly 6.6 hours of retained history.
- **Date identified:** 2026-08-10

### Cause

`steamwebhelper` (the Chromium instance embedded in the Steam client) performs
DIAL/Cast device discovery, broadcasting an SSDP `M-SEARCH` to
`239.255.255.250:1900` every two minutes from a fresh ephemeral source port.
LAN devices answer by **unicast** back to that ephemeral port — here the
Philips Hue Bridge at 192.168.1.63 (replying from port 1900) and an Android
device at 192.168.1.56 (replying from a random port).

conntrack cannot associate those replies with the request, because the request
was addressed to a multicast group rather than to the responder. Every reply
therefore falls through to the default deny and is logged. `before.rules`
already ACCEPTs inbound SSDP to `239.255.255.250:1900`, but these replies are
addressed to the host's own unicast IP and never match that rule.

Confirmed by correlation: a transient socket on `192.168.1.81:59526` held by
`steamwebhelper` (~3s lifetime) matched `DPT=59526` in the blocks logged at
10:20:41 from both devices.

### Workaround applied

- **File:** `/etc/ufw/after.rules` (two rules appended to `ufw-after-input`)
- **Managed by:** `run_onchange_before_silence-ufw-ssdp-log-spam-mimir.sh.tmpl`
- Uses ufw's own `ufw-skip-to-policy-input` target, the same mechanism ufw
  ships for NetBIOS/DHCP/broadcast noise — it jumps to a bare `DROP` with no
  `LOG` target.

**This is a logging-only change.** The packets were already denied by the
default policy and still are; nothing new is exposed and no traffic that
previously reached the host is affected.

The second rule (`-s 192.168.1.0/24 -p udp --dport 32768:60999`) is
deliberately broad, because SSDP responders may answer from any source port
and there is no port signature to match on. It silences all LAN UDP addressed
to the ephemeral range. Those packets were already dropped; the only loss is
that they no longer appear in the firewall log.

### Check condition

Revisit if Steam stops performing DIAL/Cast discovery, or if firewall logs for
LAN UDP to ephemeral ports are ever needed for diagnostics. To reverse:

```bash
sudo sed -i '/BEGIN chezmoi: silence SSDP/,/END chezmoi: silence SSDP/d' /etc/ufw/after.rules
sudo ufw reload
```
