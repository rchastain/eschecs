
*Eschecs* is a free chess playing program, which uses the [UCI protocol](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html) to dialog with UCI chess engines.

*Eschecs* includes an original UCI engine called *Moustique*. *Eschecs* and *Moustique* are open source programs written in Pascal.

## I. User's Guide

### A. Moving a piece

To move a piece, drag it with the mouse to the target square.

### B. Artificial Chess Player

A default chess engine (Fruit or the last engine used) is loaded at application starts. You can select another opponent in a list of chess engines. See the "Moves" menu.

### C. Play Mode

The default play mode is user versus computer. The user plays white. If you wish the computer to play white, click on "Computer move".

To play in human versus human mode, unselect the "Autoplay" option in the "Moves" menu. 

### D. Saving Game and Options

When you quit *Eschecs* by clicking on the "Quit" menu item or by pressing the ESC key, the game and the options are automatically saved.

### E. Keyboard Controls

    ↑   Go to the last position
    ↓   Go back to the first position
    ←   Go back to the previous position
    →   Go to the next position
    ESC Close the application

## II. Advanced User's Guide

### A. Installation of a new UCI engine.

To install a new engine, you have to edit **engines.json**.

### B. Protocol.

To do.

## III. Programmer's Guide.

### A. Compilation

*Eschecs* is an open source program for the Free Pascal Compiler. *Eschecs* uses [fpGUI][1] and [BGRABitmap][2] libraries.

## IV. Credits

The [320 chessboard](https://raw.githubusercontent.com/rchastain/eschecs/master/styles/0.png) (with its pieces) is a reproduction of the game [Fritz 1.0]. *Fritz 1.0* is a chess program by Mathias Feist, Frans Morsch and Mathias Wüllenweber.

The [640 chessboard](https://raw.githubusercontent.com/rchastain/eschecs/master/styles/4.png) is the [work of Daniela Di Lena](https://dilena.de/chess-artwork-pieces-and-board-art-assets).

The pieces from the [480 chessboard](https://raw.githubusercontent.com/rchastain/eschecs/master/styles/3.png) comes from the Chess Mark font by Armando Marroquin.

The application icon is the white king of the [Chess Montreal font](http://alcor.concordia.ca/~gpkatch/montreal_font.html) by Gary Katch.

Fruit 2.1 is a chess program by Fabien Letouzey.

## V. Author

*Eschecs* is a Pascal program by Roland Chastain.

Contributors: Johann Elsass, Fred van Stappen.

## V. Website

<https://sites.google.com/view/eschecs/accueil> 

[1]: https://github.com/graemeg/fpGUI 
[2]: https://github.com/bgrabitmap/bgrabitmap
[Fritz 1.0]: http://www.top-5000.nl/cp.htm

