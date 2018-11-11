
*Eschecs* is a free chess playing program, which uses the [UCI protocol](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html) to dialog with UCI chess engines.

*Eschecs* includes an original UCI engine called *Moustique*.

*Eschecs* and *Moustique* are open source programs written in Pascal.

## I. User's Guide

### A. Moving a piece

To move a piece, drag it with the mouse to the target square.

### B. Artificial Chess Player

After you have started the application, you have to choose your opponent in a list. See the "Moves" menu. As long as you don't do that, the computer will not play!

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

To install a new engine, you have to edit **engines.json**, or to create another JSON file and starting *Eschecs* with the name of that file as first parameter.

### B. Protocol.

To do.

## III. Programmer's Guide.

### A. Operating System.

In its current implementation, *Eschecs* can only be compiled for the Windows system, because the **ProcessUtils** unit is based upon the **Windows** unit.

### B. Compilation

*Eschecs* is an open source program for the Free Pascal Compiler.

The graphical interface of *Eschecs* is based upon [fpGUI][1] and [BGRABitmap][2] libraries.

To build *Eschecs*, double click on **comspec.cmd**. Type "build.lib" to compile fpGUI, LazUtils and BGRABitmap.

Type "build" to compile *Eschecs*. Type "build english" if your want the english version.

The compiler path must be known by the system. Or else, if you have got the Lazarus IDE, you can open the **eschecs.lpi** project, which you can find in **sources** directory.

## IV. Credits

Pieces pictures are a reproduction of [Fritz 1.0].

*Fritz 1.0* is a chess program by Mathias Feist, Frans Morsch and Mathias Wüllenweber.

The application icon is the white king of the Chess Montreal font by Gary Katch.

<http://alcor.concordia.ca/~gpkatch/montreal_font.html>

Fruit 2.1 is a chess program by Fabien Letouzey.

## V. Author

*Eschecs* is a Pascal program by Roland Chastain (eschecs2018@gmail.com).

## V. Website

<https://sites.google.com/view/eschecs/accueil> 

[1]: https://github.com/graemeg/fpGUI 
[2]: https://github.com/bgrabitmap/bgrabitmap
[Fritz 1.0]: http://www.top-5000.nl/cp.htm
