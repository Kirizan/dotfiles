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

## USB-001: GenesysLogic USB hub autosuspend causes device disconnects

- **Status:** Active (hardware/firmware limitation)
- **Affected systems:** mimir
- **Symptoms:** GenesysLogic USB 2.1 Hub (05e3:0610) repeatedly disconnects
  and reconnects, taking downstream devices (Logitech receiver, keyboard) with
  it. Device numbers climb rapidly (50+ re-enumerations per boot).
- **Date identified:** 2026-02-05

### Workaround applied

- **File:** `/etc/udev/rules.d/90-usb-fixes.rules` (disables autosuspend)
- **Managed by:** `run_once_before_fix-usb-mimir.sh.tmpl`

### Check condition

Replace the hub with one that properly supports USB autosuspend, or check for
a firmware update from GenesysLogic. Alternatively, a kernel update that
improves xHCI handling for this chipset would resolve it.

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
