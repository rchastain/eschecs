
# Eschecs

## Overview

*Eschecs* is a simple graphical interface to play chess against [UCI](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html) engines.

## Usage

*Eschecs* is shipped with an engine (Cheng). If you wish to use another one, start *Eschecs* with the engine path as parameter.

```
./eschecs /home/robert/uciengines/CT800_V1.42
```

You can find [here](https://github.com/rchastain/eschecs/blob/master/ENGINES.md) a collection of UCI engines that you could use with *Eschecs*.

## Screenshot

![alt text](images/screenshots/eschecs510a.png)

You can see more screenshots on [this page](https://github.com/rchastain/eschecs/blob/master/images/screenshots/README.md).

## Command line options

The behaviour and the appearance of *Eschecs* can be set using the following command line parameters.

| Parameter name | Parameter value |
| --- | --- |
| `-p <v>`, `--position=<v>` | The position to be loaded, in FEN format. |
| `-a <v>`, `--autoplay=<v>` | The computer will be the second player. Possible values: **false**, **true**. |
| `-t <v>`, `--time=<v>` | Time allowed for the computer move, in milliseconds. |
| `-u <v>`, `--upsidedown=<v>` | Draw the chessboard upside down. Possible values: **false**, **true**. |
| `-c <v>`, `--chessboard=<v>` | Appearance of the chessboard. Possible values: **simple**, **marble**, **marblenew**, **marblecustom**, **wood**. |
| `-m <v>`, `--marblecolors=<v>` | Marble colors, in **RRGGBBAA,RRGGBBAA,RRGGBBAA,RRGGBBAA** format. |
| `-f <v>`, `--font=<v>` | Piece set. See below possible values. |
| `-l <v>`, `--language=<v>` | Language. See below possible values. |
| `-s <v>`, `--size=<v>` | Size of the square. See below possible values for each font. |
| `-w <v>`, `--white=<v>` | Color of white squares, in **RRGGBBAA** format. |
| `-b <v>`, `--black=<v>` | Color of black squares, same format. |
| `-g <v>`, `--green=<v>` | Color for legal target squares. |
| `-r <v>`, `--red=<v>` | Color for the square of a king being in check. |

See *start.sh* for command line examples.

The settings will be automatically saved when you close the application, and reloaded when you start it.

### Available fonts and sizes

```
Alpha    30, 40, 50, 60, 70, 80, 90, 100
Condal   30, 40, 50, 60, 70, 80, 90, 100
Line     30, 40, 50, 60, 70, 80, 90, 100
Lucena   30, 40, 50, 60, 70, 80, 90, 100
Magnetic 30, 40, 50, 60, 70, 80, 90, 100
Mark     30, 40, 50, 60, 70, 80, 90, 100
Montreal 30, 40, 50, 60, 70, 80, 90, 100
Usual    30, 40, 50, 60, 70, 80, 90, 100
Wood     30, 40, 50, 60, 70, 80
```

### Available languages

* Czech
* Dutch
* English
* French
* German
* Italian
* Spanish

Thanks to the translators:

* Martin Sedlak (Czech)
* Jean-Luc Gofflot (Dutch)
* Users of the German [Lazarus forum](https://www.lazarusforum.de/index.php) (German)
* Marcello Basso (Italian)
* Ñuño Martínez (Spanish)

## Keyboard Controls

    ↑   Go to the last position
    ↓   Go back to the first position
    ←   Go back to the previous position
    →   Go to the next position
    ESC Close the application

## Technical informations

*Eschecs* is a Pascal program. If you wish to compile it yourself, you will need the Free Pascal compiler, and the following libraries:

* [fpGUI](https://github.com/graemeg/fpGUI)
* [BGRABitmap](https://github.com/bgrabitmap/bgrabitmap)
* LazUtils from the [Lazarus](https://sourceforge.net/projects/lazarus/) library
* [uos](https://github.com/fredvs/uos)

An easy method to build the program is to open a terminal in *source* folder and type `make`. But before doing that, you will have to edit libraries path in *extrafpc.cfg*. (The provided *Makefile* has been made for Linux. It you try to compile on another OS, you could have to retouch it.)

You can also create a Lazarus or an MSEide project.

## Authors

*Eschecs* is a Pascal program by Roland Chastain, with contributions by Johann Elsass and Fred van Stappen.

## Credits

### Graphics

The application icon is the white king of the [Chess Montreal font](http://alcor.concordia.ca/~gpkatch/montreal_font.html).

The wood chessboard and its pieces are the work of [Daniela Di Lena](https://dilena.de/chess-artwork-pieces-and-board-art-assets).

The other pieces set has been made from TrueType chess fonts:

* *Chess Alpha* by Eric Bentzen
* *Chess Condal, Chess Line, Chess Mark* by Armando Marroquin
* *Chess Montreal* by Gary Katch

### Sounds

The sound effects come from [Lichess][1], the well-known chess server by Thibault Duplessis, and from [The Essential Retro Video Game Sound Effects Collection][2] by Juhani Junkala.

[1]: https://github.com/ornicar/lila/tree/master/public/sound
[2]: https://opengameart.org/content/512-sound-effects-8-bit-style
