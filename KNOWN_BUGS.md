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
