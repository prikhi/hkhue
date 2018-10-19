# hkhue

A Haskell library & application for controlling Philips Hue lights.


## Status

There is a daemon & CLI client, which currently supports:

* Identifying lights by blinking them on & off.
* Reseting all the lights to their default color temperature & brightness.
* Setting the Brightness & Color-Temperature/RGB of a specific light or all
  lights.


## Ideas

Dunno exactly what I want but probably most of this:

* Basic usage
  * Light renaming
  * Create/run preset scenes
  * Toggle light(s) on/off.
  * Set light color(s) (brightness, RGB or Color Temp) w/ transition time
    * Properly handle on/off w/ transition times:
       https://developers.meethue.com/content/brightness-turns-down-1-automatically-shortly-after-sending-signal-hue-bug
  * Global/Per-Light increments (brightness & color temp)
  * Dameon & CLI Client Config files - daemon needs bridge host, bind address,
    & port; client needs daemon address and port
* Long/Constant effects
  * Slowly brighten/dim over X number of minutes
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
* Not so urgent
  * Similar commands for controlling groups
* Management Daemon
  * Listens for client commands and send appropriate message to Hue hub
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

Then you can control the lights via cli:
```
stack exec hkhue -- set-all -b 40 -c 255,0,255
```

To see all available commands, run `stack exec hkhue -- --help`. Run `stack
install` to install to `~/.local/bin/`:

```
stack install
PATH="~/.local/bin:${PATH}"
hkhue --help
```


## License

GPL-3.0
