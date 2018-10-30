# hkhue

A Haskell library & application for controlling Philips Hue lights.


## Status

There is a daemon & CLI client, which currently supports:

* Identifying lights by blinking them on & off.
* Reseting all the lights to their default color temperature & brightness.
* Setting the name of a light.
* Setting the Brightness & Color-Temperature/RGB of a specific light(by name or
  number) or all lights - with custom light-state transition times.
* Scanning for new lights & associating them with the bridge.
* Syncing the color temperature of your lights to redshift.


## Ideas

Dunno exactly what I want but probably most of this:

* Basic usage
  * Query light status - id, name, on/off, color, brightness
  * Create/run preset scenes
    * Scenes on Hue Bridge or in own database?
    * Play scene but don't change current brightness!
  * Global/Per-Light increments (brightness & color temp)
  * Specify multiple lights in `set-light` command
  * Dameon Config file
    * Support overriding config options w/ cli flags
  * CLI Config file
    * Support overriding config options w/ cli flags
  * Add in-depth mode descriptions to CLI `--help` docs
  * Nicer transition-time flag for CLI client - add support for values
    like "1s", "30m", "2h"
  * Show errors! "Can't reach bridge", "Can't reach daemon", "Invalid color
    channel value", etc.
    * On client socket exception, print warning & re-run client mode
  * Refactor DaemonState into separate module(as opaque type?). Ensure all Hue
    API requests update the daemonBridgeState. E.g, so we can set name &
    immediately use it instead of having to wait for bridge sync.
  * Clean Up Daemon Output, Support Log File or Verbosity Switches?
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
hkhue set-all --on --color-temperature 2500 --brightness 75 --transition-time 300
hkhue alert 1
hkhue set-light 1 --color 255,0,255 -b 100 -t 100
hkhue alert 2
hkhue set-name 2 desk
# Sync redshift in the background
hkhue redshift --interval 20 &
# Transition to 6000K over 10 minutes
hkhue set-light desk -k 6000 -t "$((10 * 60 * 10))"
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

## License

GPL-3.0
