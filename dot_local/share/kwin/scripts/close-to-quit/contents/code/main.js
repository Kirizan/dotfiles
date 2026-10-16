// close-to-quit: make closing an app's last window quit the app.
//
// Many apps (Electron, Flutter, ...) turn the X button into "hide to tray" with
// no setting to change it. Most of them draw their own X, so KWin never sees
// the click; what it sees is the app's last normal window going away. When that
// happens, wait a moment (splash screens and window swaps re-map quickly), and
// if the process still has no windows hand its pid to close-to-quit@<pid>.service.
// The helper decides: it only acts on apps still sitting in the tray and not on
// its allowlist.

const GRACE_MS = 3000;
const pending = new Set(); // keep timers referenced until they fire

function hasWindows(pid) {
    return workspace.windowList().some(w => w.pid === pid && w.normalWindow);
}

workspace.windowRemoved.connect(window => {
    if (!window.normalWindow || window.transient || !(window.pid > 0)) {
        return;
    }
    const pid = window.pid;
    const timer = new QTimer();
    timer.singleShot = true;
    timer.interval = GRACE_MS;
    timer.timeout.connect(() => {
        pending.delete(timer);
        if (!hasWindows(pid)) {
            callDBus("org.freedesktop.systemd1", "/org/freedesktop/systemd1",
                "org.freedesktop.systemd1.Manager", "StartUnit",
                "close-to-quit@" + pid + ".service", "replace");
        }
    });
    pending.add(timer);
    timer.start();
});
