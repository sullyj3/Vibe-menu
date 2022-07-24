# vibe-menu
Control your vibrators from the console!
This program functions as a basic demonstration for `[buttplug-hs-core](https://github.com/sullyj3/buttplug-hs-core)`
It also functions as a testbed for experimental `buttplug-hs-core` apis, on branches other than `main`.

Compatibility:
- Linux: yes
- Windows: WSL only
- Mac: probably (let me know so I can update this!)

## Installation:
1. Make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) installed
2.

    ```
    $ cd Vibe-menu
    $ stack install
    ```

3. make sure the `stack install` location (`$ stack path --local-bin`) is in your `$PATH`

## Usage
1. Start a buttplug server listening on insecure websockets.
2.

    ```
    $ vibe-menu
    ```

The number keys control vibration speed for the highlighted device, with 1 = 10%, 0 = 100%.

`s` stops all connected devices.
