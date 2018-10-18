# hkhue

A Haskell library & application for controlling Philips Hue lights.


## Status

There is a shell of a daemon & CLI client, you can currently:

* Set the brightness of a specific light
* Set the brightness & color of all lights using an RGB value.


## Ideas

Dunno exactly what I want but probably most of this:

* Basic usage
  * Light identifier(blink the specified light on/off for 2-3 seconds)
  * Light renaming
  * Create/run preset scenes
  * Manually set each light (with either RGB & Color Temp)
  * Set all lights (brightness, RGB or Color Temp)
  * Global/Per-Light brightness adjustment
* Long/Constant effects
  * Slowly brighten/dim over X number of minutes
  * Constant slow fades between colors
  * Breathing
  * Flickering/Candlelight
  * Party mode
  * Sound reactive
  * Color based on computer monitor
* Management Daemon
  * Listens for client commands and send appropriate message to Hue hub
  * Database for storing color bank, custom effects, scene
  * Manages & cancels long-running effects
  * Config file - just need bridge host/ip for now
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
