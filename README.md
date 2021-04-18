
# Eschecs

## Overview

*Eschecs* is a graphical interface to play chess against [UCI](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html) engines.

## Usage

*Eschecs* is shipped with an engine. If you wish to use another engine, start *Eschecs* with the engine path as parameter.

### Command line options

| Parameter name | Parameter value |
| --- | --- |
| -p \<v\>, --position=\<v\> | The position to be loaded, in FEN format. |
| -a \<v\>, --autoplay=\<v\> | The computer will be the second player. Possible values: false, true. |
| -u \<v\>, --upsidedown=\<v\> | Draw the chessboard upside down. Possible values: false, true. |
| -c \<v\>, --chessboard=\<v\> | Appearance of the chessboard. Possible values: simple, marble, marble2, wood. |
| -m \<v\>, --movetime=\<v\> | Time allowed for the computer move, in milliseconds. |
| -f \<v\>, --font=\<v\> | Piece set. See below possible values. |
| -l \<v\>, --language=\<v\> | Language. See below possible values. |
| -s \<v\>, --size=\<v\> | Size of the square. See below possible values for each font. |
| -f \<v\>, --fischerandom=\<v\> | Play Fischer Random Chess. Possible values: false, true. |
| -w \<v\>, --white=\<v\> | Color of white squares, in RRGGBBAA format. |
| -b \<v\>, --black=\<v\> | Color of black squares. |
| -g \<v\>, --green=\<v\> | Color of legal target squares. |
| -r \<v\>, --red=\<v\> | Color of white squares. |

#### Available fonts and sizes

* Alpha    30, 40, 50, 60, 70, 80, 90, 100
* Condal   30, 40, 50, 60, 70, 80, 90, 100
* Line     30, 40, 50, 60, 70, 80, 90, 100
* Lucena   30, 40, 50, 60, 70, 80, 90, 100
* Magnetic 30, 40, 50, 60, 70, 80, 90, 100
* Mark     30, 40, 50, 60, 70, 80, 90, 100
* Montreal 30, 40, 50, 60, 70, 80, 90, 100
* Usual    30, 40, 50, 60, 70, 80, 90, 100
* Wood     30, 40, 50, 60, 70, 80

#### Available languages

* Czech
* Dutch
* English
* French
* German
* Italian
* Spanish

## Technical informations

*Eschecs* is a Pascal program. If you wish to compile it yourself, you will need fpGUI, BGRABitmap and uos libraries.

## Screenshot

![alt text](images/screenshots/eschecs500f.png)
