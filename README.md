# vibe-menu
Control your vibrators from the console!
This program is a testing ground for [buttplug-hs-core](https://github.com/sullyj3/buttplug-hs-core)

## Installation:
1. Make sure you have [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) installed
2.

```
$ cd Vibe-menu
$ stack install
```
3. make sure the stack install location (`$ stack path --local-bin`) is in your `$PATH`

## Usage
1. Start a buttplug server listening on insecure websockets.
2.
```
$ vibe-menu
```

The number keys control vibration speed for the highlighted device, with 1 = 10%, 0 = 100%.

`s` stops all connected devices.
