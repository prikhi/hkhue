# hkhue

A Haskell library & application for controlling Philips Hue lights.


## Status

There is a daemon & CLI client, which currently supports:

* Identifying lights by blinking them on & off.
* Reseting all the lights to their default color temperature & brightness.
* Setting the Brightness & Color-Temperature/RGB of a specific light or all
  lights - with custom light-state transition times.


## Ideas

Dunno exactly what I want but probably most of this:

* Basic usage
  * Query light status - id, name, on/off, color, brightness
  * Support using light names as arguments instead of ID numbers
    * Change flag to string type, check if name first, if not & all digits
      check if valid id.
    * Keep cache of name->id in daemon, reference when passed name as arg, if
      not exists, update cache & check again, print error if not
  * Create/run preset scenes
    * Scenes on Hue Bridge or in own database?
    * Play scene but don't change current brightness!
  * Global/Per-Light increments (brightness & color temp)
  * Dameon Config file
    * Bridge Host
    * Bind Address/Port
    * Bridge Sync Interval
  * CLI Config file
    * Daemon Address/Port
    * Daemon Port
  * Add in-depth mode descriptions to CLI `--help` docs
  * Nicer transition-time flag for CLI client - add support for values
    like "1s", "30m", "2h"
  * Show errors! "Can't reach bridge", "Can't reach daemon", "Invalid color
    channel value", etc.
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
  * Match Redshift(see `redshift -p | grep Temp`)
    * Make CLI command that syncs redshift w/ current color temp? CLI command
      instead of daemon command would let us keep remote redshift instances in
      sync.
      * Pick light or average last color temp of every light.
* Not so urgent
  * Similar commands for controlling groups
  * Better bridge pairing flow(prompt user through UI apps, not daemon),
    autodiscovering
  * Support multiple bridges & cross-bridge scenes
* Code cleanup
  * More function/API docs
  * Add CLI examples to README
  * Add types for things like LightId, Brightness, Color Temp, &
    Transition  values.
* Management Daemon
  * Heartbeat that pulls full state from bridge every 30-120 seconds &
    light state in smaller intervals. Use values as a cache to improve bridge
    performance.
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

```
stack build
stack exec hkhued -- <bridge-ip>
```

Then you can control the lights with the CLI client:

```
stack exec hkhue -- set-all -b 40 -c 255,0,255
```

Run `stack install` to install to `~/.local/bin/`:

```
stack install
PATH="~/.local/bin:${PATH}"
hkhue set-all --on --color-temperature 2500 --brightness 75 --transition-time 300
hkhue alert 1
hkhue set-light 1 --color 255,0,255 -b 100 -t 100
```

To see all available commands & flags, run `hkhue --help` or `hkhue <command>
--help`.

## License

GPL-3.0
