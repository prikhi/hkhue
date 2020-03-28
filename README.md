# hkhue

A Haskell library & application for controlling Philips Hue lights.


## Status

There is a daemon & CLI client, which currently supports:

* Showing the current status of each light(name, on/off, color, brightness).
* Identifying lights by blinking them on & off.
* Reseting all the lights to their default color temperature & brightness.
* Setting the name of a light.
* Setting the Brightness & Color-Temperature/RGB of specific lights(by name or
  number) or all lights - with custom light-state transition times.
* Scanning for new lights & associating them with the bridge.
* Syncing the color temperature of your lights to redshift.


## Ideas

Dunno exactly what I want but probably most of this:

* Basic usage
  * Query light status
    * Improve the XY -> RGB conversion - sometimes the values are negative...
  * Create/run preset scenes
    * Scenes on Hue Bridge or in own database?
    * Play scene but don't change current brightness!
  * Global/Per-Light increments (brightness & color temp)
  * Dameon Config file
    * Support overriding config options w/ cli flags
  * CLI Config file
    * Support overriding config options w/ cli flags
  * Add in-depth mode descriptions to CLI `--help` docs
  * Nicer transition-time flag for CLI client - add support for values
    like "1s", "30m", "2h"
  * Show errors! "Can't reach bridge", "Can't reach daemon", "Invalid color
    channel value", etc.
  * Refactor DaemonState into separate module(as opaque type?). Ensure all Hue
    API requests update the daemonBridgeState. E.g, so we can set name &
    immediately use it instead of having to wait for bridge sync.
  * Clean Up Daemon Output, Support Log File or Verbosity Switches?
  * Redshift syncing
    * when making large ct jumps but with long transition times, the bridge
      will return the final temperature for the first few seconds, then return
      the actual current color temp. This causes redshift to flash from the
      current temp to the target color temp and back. We should track when we
      set the color temp w/ long transitions and modify the color temp we give
      to the redshift-syncing client so that there is no initial flash. Maybe
      lock the color temperature from being updated for 5-10 seconds(check how
      long we need) or simply override the temperature for the average
      calculations for some period of time.
* Long/Constant effects
  * Slowly brighten/dim over X number of minutes
    * Currently have a script that slowly increases brightness & color
      temp(at variable rates between various intervals) as I do my
      morning routine. Ideally, would be customizable in web ui &
      present graph of colortemp vs time. Manual trigger lets me sync it
      to my schedule instead of the daylight schedule, but it should act as the
      "base" effect. De-activating an effect or scene should return the state
      to the base effect.
  * Constant slow fades between colors
  * Breathing(brightness sine-wave)
  * Flickering/Candlelight(brightness randomization?)
  * Party mode
  * Sound reactive
  * Color based on computer monitor
* Not so urgent
  * Similar commands for controlling groups
  * Better bridge pairing flow(prompt user through UI apps, not daemon),
    autodiscovering
  * Support multiple bridges & cross-bridge scenes
  * `scan` command should wait 40s & print out any newly discovered lights.
  * More `redshift` flags:
    * `--single-light LIGHT_ID`
    * `--on-only`
    * `--pull` - instead of pushing color temp to redshift, pull color temp
      from redshift
* Code cleanup
  * More function/API docs
  * Add types for things like PercentBrightness, HueBrightness(1-254),
    ByteChannel(0-255), UnitChannel(0-1), TransitionTime, etc.
* Management Daemon
  * Database for storing color bank, custom effects, scene
  * Manages & cancels long-running effects
* A GUI for manual control & CLI for scripting
  * Elm for Web GUI?
  * Websockets for Elm, Sockets for CLI? Or websockets for both?
  * Color picker with live previews(toggle full room & single light)
  * CLI mostly for scripting/playback(e.g., dim all by 10%), less important for
    light setup/configuration?
* Support all API features since I don't use the Hue app
* Eventually support the Hue Entertainment Streaming API


## Build / Run

Build the applications & start the daemon:

```sh
stack build
stack exec hkhued
```

Then you can control the lights with the CLI client:

```sh
stack exec hkhue -- set-all -b 40 -c 255,0,255
```

Run `stack install` to install the binaries to `~/.local/bin/`:

```sh
stack install
PATH="~/.local/bin:${PATH}"
hkhue set --on --color-temperature 2500 --brightness 75 --transition-time 300
hkhue alert 1
hkhue rename 1 ceiling
hkhue set ceiling --color 255,0,255 -b 100 -t 100
hkhue alert 2
hkhue rename 2 desk
# Sync redshift in the background
hkhue redshift --interval 20 &
# Transition to 6000K over 10 minutes
hkhue set ceiling desk -k 6000 -t "$((10 * 60 * 10))"
```

You can install just the daemon or client by running `stack install
hkhue:exe:hkhue` or `stack install hkhue:exe:hkhued`.

To see all available commands & flags, run `hkhue --help` or `hkhue <command>
--help`.


## Configuration

You can modify the default behavior of the daemon & CLI client by creating a
config file at `~/.config/hkhue/config.yaml`. Global options are defined at the
top level while daemon & client options are nested under their respective keys.
All values are optional and will fall back to the following defaults:

```yaml
# The Address of the Daemon
bind-address: 0.0.0.0
# The Port for the Daemon to Use
bind-port: 9160

daemon:
    # The Hostname or Address of the Hue Bridge
    bridge-host: philips-hue
    # Seconds Between Full Bridge State Cache Resyncs
    bridge-sync-interval: 60
    # Seconds Between Light State Cache Resyncs
    lights-sync-interval: 5
```


## Examples

### Slowly Ramping Up Color Temperature

This script uses an array of (Color Temperature, Brightness, & Minutes) to
slowly ramp up the light intensity as you wake up. See the `/examples/wakeup/`
folder for a Haskell implementation.

```sh
#!/usr/bin/env bash
#
# Gradually brighten & increase color temperature to ease waking up.
# Starts at pure red & gets to 6500K in ~60 minutes.

echo "[$(date +%T)] Starting wake up sequence."

echo "[$(date +%T)] Turning on red lights at lowest brightness."
hkhue set --on --wait --color 255,0,0 --brightness 1

TEMP_RAMP=(
  # KELVIN  BRIGHT% MINUTES
    2000     10      5
    2500     15      5
    3000     30      7
    3500     60      7
    4250     80     10
    5000    100     10
    6500    100     15
)
RAMP_LEN=${#TEMP_RAMP[@]}

for (( INDEX=0; INDEX < RAMP_LEN; INDEX=INDEX+3 )); do
    COLOR_TEMP="${TEMP_RAMP[${INDEX}]}"
    BRIGHTNESS="${TEMP_RAMP[$((INDEX+1))]}"
    MINUTES="${TEMP_RAMP[$((INDEX+2))]}"
    TRANSITION_TIME="$(( 10 * 60 * MINUTES))"
    hkhue set --wait \
        -k "${COLOR_TEMP}" \
        -b "${BRIGHTNESS}" \
        -t "${TRANSITION_TIME}"
    echo "[$(date +%T)] Reached ${COLOR_TEMP}K and ${BRIGHTNESS}% brightness" \
         "in ${MINUTES} minute(s)."
done

echo "[$(date +%T)] Wake up sequence complete."
```

### Custom Color Loops

This script will loop through whichever RGB colors you like.

```sh
#!/usr/bin/env bash
#
# Loop through the specified RGB colors forever.
SECONDS=5
TRANSITION_TIME="$(( 10 * SECONDS ))"
COLORS=(
  # RED GRN BLU
    255   0 255     # Magenta
      0 255 255     # Cyan
    255 127   0     # Orange
      0 255   0     # Green
)
COLORS_LEN=${#COLORS[@]}
while true; do
    for (( INDEX=0; INDEX < COLORS_LEN; INDEX=INDEX+3 )); do
        RED="${COLORS[${INDEX}]}"
        GREEN="${COLORS[$((INDEX+1))]}"
        BLUE="${COLORS[$((INDEX+2))]}"
        hkhue set --wait \
            -c "${RED},${GREEN},${BLUE}" \
            -t "${TRANSITION_TIME}"
    done
done
```


## License

GPL-3.0
