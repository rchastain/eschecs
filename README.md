*Eschecs* is a chess playing program, using the [UCI protocol](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html) to dialog with a chess engine.

![alt text](screenshots/eschecs500d.png)

## I. User's Guide

### A. Moving a piece

To move a piece, drag it with the mouse to the target square.

### B. Artificial Chess Player

A default chess engine is loaded at application start.

You can select another opponent in a list of chess engines. See the "Moves" menu.

### C. Play Mode

The default play mode is user versus computer, user playing white.

If you wish the computer to play white, click on "Computer move".

To play in human versus human mode, uncheck the "Autoplay" option in the "Moves" menu. 

### E. Keyboard Controls

    ↑   Go to the last position
    ↓   Go back to the first position
    ←   Go back to the previous position
    →   Go to the next position
    ESC Close the application

## II. Advanced User's Guide

### A. Installation of a new UCI engine

To install a new engine, you have to edit **eschecs.eng**. That file stands in the **config** directory.

### B. Protocol

*Eschecs* uses the UCI protocol.

For the "go" command, *Eschecs* uses the following syntax:

    go movetime 1000

You can change the value of the parameter by editing **eschecs.ini**, which stands in the **config** directory.

## III. Programmer's Guide

### A. Compilation

*Eschecs* is an open source program for the Free Pascal Compiler.

*Eschecs* uses [fpGUI][1], [BGRABitmap][2] and [uos][3] libraries.

## IV. Credits

### A. Chess pieces pictures

The wood chessboard and its pieces are the work of [Daniela Di Lena](https://dilena.de/chess-artwork-pieces-and-board-art-assets).

The application icon is the white king of the [Chess Montreal font](http://alcor.concordia.ca/~gpkatch/montreal_font.html). *Chess Montreal* is a TrueType font by Gary Katch.

*Chess Alpha* is a TrueType font by Eric Bentzen. *Chess Condal, Chess Line, Chess Mark* are TrueType fonts by Armando Marroquin.

### B. Sound effects

Sound effects come from [Lichess](https://github.com/ornicar/lila/tree/master/public/sound), the well-known chess server by Thibault Duplessis, or from [The Essential Retro Video Game Sound Effects Collection](https://opengameart.org/content/512-sound-effects-8-bit-style) by Juhani Junkala.

### C. Chess engines

*Fruit 2.1* is a chess program by Fabien Letouzey. *Moustique* is a UCI engine by Roland Chastain, based on *Schachspiel* by Jürgen Schlottke.

See [this page](https://github.com/rchastain/eschecs/blob/master/ENGINES.md) for details about other engines shipped with *Eschecs*.

### D. Libraries

[fpGUI](https://github.com/graemeg/fpGUI) is a cross-platform GUI toolkit using Free Pascal, by Graeme Geldenhuys.

[BGRABitmap](https://github.com/bgrabitmap/bgrabitmap) is a graphics library by Johann Elsass.

[uos](https://github.com/fredvs/uos) is a collection of audio libraries with united procedures by Fred van Stappen.

### E. Contributors

Thanks to John Bennett and to Norbert Raimund Leisner for the Moustique logo, a picture of the Farman F455 Moustique. Thanks to Johann Elsass for his graphical snippets. Thank to Fred van Stappen, for his valuable contribution concerning Unix compatibility, sound, bug fixes, and kind support.

## V. Author

*Eschecs* is a Pascal program by Roland Chastain, with contributions by Johann Elsass and Fred van Stappen.

[1]: https://github.com/graemeg/fpGUI 
[2]: https://github.com/bgrabitmap/bgrabitmap
[3]: https://github.com/fredvs/uos
