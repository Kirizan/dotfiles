---
name: steam-backup
description: Use when the user asks to "backup game saves", "restore game saves", "backup steam", "restore steam saves", "find game saves", or discusses Steam/Proton save file management, game save migration, or Windows-to-Linux game save transfers.
---

# Steam Game Save Backup & Restore

This skill manages backup and restoration of Steam game save files across Windows and Linux. It knows the save locations for all games in the user's library and can discover new games.

## Backup Location

All backups are stored at: `~/backups/steam/<Game Name>/`

## Steam Configuration

- **Steam ID:** 36250180
- **Steam Root:** `~/.steam/steam/`
- **Compatdata:** `~/.steam/steam/steamapps/compatdata/`
- **Userdata:** `~/.steam/steam/userdata/36250180/`

## Game Save Database

### Native Linux Games

These games run natively and store saves in standard Linux paths.

| Game | AppID | Linux Save Path | Backup Contents |
|------|-------|----------------|-----------------|
| Terraria | 105600 | `~/.local/share/Terraria/Players/` and `~/.local/share/Terraria/Worlds/` | Characters (.plr), worlds (.wld), backups |
| tModLoader | 1281930 | `~/.local/share/Terraria/tModLoader/` | Players, Worlds, Mods/enabled.json, ModConfigs |
| Factorio | 427520 | `~/.factorio/saves/` (also mods at `~/.factorio/mods/`) | Save .zip files, mods, blueprints, player-data.json |
| RimWorld | 294100 | `~/.config/unity3d/Ludeon Studios/RimWorld by Ludeon Studios/Saves/` | .rws save files, ideology .rid files |
| Stardew Valley | 413150 | `~/.config/StardewValley/Saves/` | Character folders with save data |
| Stellaris | 281990 | `~/.local/share/Paradox Interactive/Stellaris/save games/` | Campaign folders with .sav files |
| shapez 2 | 2162800 | `~/.config/unity3d/tobspr Games/shapez 2/` | savegames/, blueprints/ |

### Proton Games

These games run via Proton. Save paths are under the Proton prefix.

| Game | AppID | Proton Save Path (relative to compatdata prefix) | Backup Contents |
|------|-------|--------------------------------------------------|-----------------|
| Cyberpunk 2077 | 1091500 | `pfx/drive_c/users/steamuser/Saved Games/CD Projekt Red/Cyberpunk 2077/` | Save slots with sav.dat, metadata, screenshots |
| Satisfactory | 526870 | `pfx/drive_c/users/steamuser/AppData/Local/FactoryGame/Saved/SaveGames/` | .sav files |
| Captain of Industry | 1594320 | `pfx/drive_c/users/steamuser/AppData/Roaming/Captain of Industry/Saves/` | .save files, Blueprints/ |
| Hogwarts Legacy | 990080 | `pfx/drive_c/users/steamuser/AppData/Local/Hogwarts Legacy/Saved/SaveGames/` | .sav files |
| Titan Quest - Immortal Throne | 475150 | `pfx/drive_c/users/steamuser/Documents/My Games/Titan Quest - Immortal Throne/SaveData/` | Player.chr, map data |
| Erenshor | 2382520 | `pfx/drive_c/users/steamuser/AppData/LocalLow/Burgee Media/Erenshor/ESSaveData/` | Character slots, NPC sim data |
| BALL x PIT | 2062430 | `pfx/drive_c/users/steamuser/AppData/LocalLow/Kenny Sun/BALL x PIT/` | .yankai save files |
| Little Rocket Lab | 2451100 | `pfx/drive_c/users/steamuser/AppData/LocalLow/TeenageAstronauts/LittleRocketLab/` | .save and .backup files |
| DOOM The Dark Ages | 3017860 | Steam Cloud: `userdata/36250180/3017860/remote/` | Autosave slots, profile |
| Path of Exile 2 | 2694490 | `pfx/drive_c/users/steamuser/Documents/My Games/Path of Exile 2/` | Hideout layouts, item filters |

### Windows Source Paths (for migration from Windows partition)

When migrating from a mounted Windows partition (e.g., `/run/media/kirizan/OS`):

```
Game                          Windows Path (under Users/Nick/)
─────────────────────────────────────────────────────────────
Terraria                      Documents/My Games/Terraria/
tModLoader                    Documents/My Games/Terraria/tModLoader/
Factorio                      AppData/Roaming/Factorio/
RimWorld                      AppData/LocalLow/Ludeon Studios/RimWorld by Ludeon Studios/Saves/
Stardew Valley                AppData/Roaming/StardewValley/Saves/
Satisfactory                  AppData/Local/FactoryGame/Saved/SaveGames/
Stellaris                     Program Files (x86)/Steam/userdata/36250180/281990/remote/save games/
shapez 2                      AppData/LocalLow/tobspr Games/shapez 2/
Captain of Industry           AppData/Roaming/Captain of Industry/
Cyberpunk 2077                Saved Games/CD Projekt Red/Cyberpunk 2077/
Hogwarts Legacy               AppData/Local/Hogwarts Legacy/Saved/SaveGames/
Titan Quest - Immortal Throne Documents/My Games/Titan Quest - Immortal Throne/SaveData/
Erenshor                      AppData/LocalLow/Burgee Media/Erenshor/ESSaveData/
BALL x PIT                    AppData/LocalLow/Kenny Sun/BALL x PIT/
Little Rocket Lab             AppData/LocalLow/TeenageAstronauts/LittleRocketLab/
DOOM The Dark Ages            Program Files (x86)/Steam/userdata/36250180/3017860/remote/
Path of Exile 2               Documents/My Games/Path of Exile 2/
```

## Operations

### Backup a Game

To back up a game's saves from Linux:

1. Identify whether the game is native or Proton from the database above
2. Build the full source path:
   - **Native:** Use the Linux Save Path directly
   - **Proton:** `~/.steam/steam/steamapps/compatdata/{APPID}/{Proton Save Path}`
   - **Steam Cloud:** `~/.steam/steam/userdata/36250180/{APPID}/remote/`
3. Copy to `~/backups/steam/<Game Name>/`:
   ```bash
   mkdir -p ~/backups/steam/"<Game Name>"
   cp -r <source_path>/* ~/backups/steam/"<Game Name>"/
   ```
4. For games with multiple save locations (e.g., Captain of Industry has Saves + Blueprints), copy each subdirectory preserving structure.

### Restore a Game

To restore saves from backup:

1. Ensure the game has been installed and launched at least once (to create the save directory structure)
2. Identify the target path from the database
3. Copy from backup, preserving structure:
   ```bash
   cp -r ~/backups/steam/"<Game Name>"/* <target_path>/
   ```
4. For Proton games, the compatdata prefix must exist (created on first launch)

### Discover New Games

To find save data for games not in the database:

1. **Check installed games:**
   ```bash
   ls ~/.steam/steam/steamapps/common/
   ```

2. **Check Proton prefixes for save data:**
   ```bash
   for appid in ~/.steam/steam/steamapps/compatdata/*/; do
     id=$(basename "$appid")
     saves=$(find "$appid/pfx/drive_c/users/steamuser/" -name "*.sav" -o -name "*.save" -o -name "*.dat" -o -name "*.wld" 2>/dev/null | head -1)
     if [ -n "$saves" ]; then
       echo "AppID $id has saves: $(dirname "$saves")"
     fi
   done
   ```

3. **Check native save locations:**
   ```bash
   # Unity games
   ls ~/.config/unity3d/
   # XDG data
   ls ~/.local/share/ | grep -iv "\.cache\|Trash\|mime\|icons\|fonts"
   ```

4. **Look up AppID to game name:**
   ```bash
   grep -l "<appid>" ~/.steam/steam/steamapps/appmanifest_*.acf
   # Or check: https://steamdb.info/app/<appid>/
   ```

5. After finding a new game's saves, add it to this skill's database by editing this SKILL.md.

### Bulk Backup All Known Games

To back up all games with existing saves:

```bash
# For each game in the database, check if saves exist and copy them
# Use the paths from the tables above
# Always use cp -r to preserve directory structure
# Always use mkdir -p to create the backup target first
```

### Migrate from Windows Partition

When a Windows partition is mounted (e.g., at `/run/media/kirizan/OS`):

1. Use the Windows Source Paths table to locate saves
2. Copy to `~/backups/steam/<Game Name>/` preserving directory structure
3. Then restore from backup to the Linux-native or Proton paths

## Important Notes

- **Always back up before restoring** - don't overwrite existing Linux saves without backing them up first
- **Proton prefix must exist** - launch the game once before restoring Proton saves
- **Terraria saves are already deployed** - they live at `~/.local/share/Terraria/` (not in backups)
- **tModLoader mods auto-download** - only `enabled.json` and `ModConfigs/` need to be backed up; actual .tmod files download from the mod browser
- **Steam Cloud games** may sync saves automatically - check before manual restore to avoid conflicts
- **File permissions** - Proton saves should be owned by the user, not root
