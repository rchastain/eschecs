
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Help', // txHelp
    'Save', // txSave
    'Quit', // txQuit
    'About Eschecs', // txAbout
    'Moves', // txMoves
    'Computer move', // txComputerMove
    'Autoplay', // txAutoPlay
    'Board', // txBoard
    'New game', // txNewGame
    'Flip board', // txFlip
    'Options', // txOptions
    'Square coloring', // txColoring
    'Sound', // txSound
    'Promotion', // txPromotion
    'Knight', // txKnight
    'Bishop', // txBishop
    'Rook', // txRook
    'Queen', // txQueen

    'To move a piece, click on it and then click on the square you wish to move it to.', // txHelp
    'Pascal chess program by Roland Chastain.', // txAbout
    'Illegal move.', // txIllegalMove
    'Le changement de style sera effectif au prochain lancement de l''application. [à traduire]', // txStyleInfo

    'White to move.', // txWhiteToMove
    'Black to move.', // txBlackToMove
    'White wins.', // txWhiteWins
    'Black wins.', // txBlackWins
    'Check!', // txCheck
    'Checkmate!', // txCheckmate
    'Stalemate!', // txStalemate
    'Draw!', // txDraw
    
    'Please wait...', // txWaiting
    'UCI protocol accepted.'#10'Engine name: %s'#10'Author: %s', // txUciOk
    'Cannot connect to the engine.' // txConnectionFailure
  );

  STYLENAME: array[TStyle] of string = (
    'Chessboard 320 Original',
    'Chessboard 480 Simple',
    'Chessboard 480 Marble',
    'Chessboard 480 New',
    'Chessboard 640 Wood'
  );
