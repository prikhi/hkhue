# hkhue

A Haskell library & application for controlling Philips Hue lights.


## Ideas

Dunno exactly what I want but probably most of this:

* Basic usage
  * Light identifier(blink the specified light on/off for 2-3 seconds)
  * Light renaming
  * Create/run preset scenes
  * Manually set each light (with either RGB & Color Temp)
  * Set all lights
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

```
stack build
stack exec hkhue -- <bridge-ip>
```


## License

GPL-3.0
