# game1

## Preparation

@see https://hackage.haskell.org/package/h-raylib-5.1.0.1#readme

ubuntu
```sh
sudo apt install freeglut3-dev
sudo apt install libx11-dev libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev
```

## Execute

* Run `stack exec -- game1-exe` to see "We're inside the application!"
* With `stack exec -- game1-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`
