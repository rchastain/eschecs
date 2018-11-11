
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Help', // txHelp
    'Quit', // txQuit
    'About Eschecs', // txAbout
    'Moves', // txMoves
    'Computer move', // txComputerMove
    'Autoplay', // txAutoPlay
    'Board', // txBoard
    'New game', // txNewGame
    'Flip board', // txFlip
    'Options', // txOptions
    'Marble board', // txMarble
    'Sound', // txSound
    'Weaker opponent', // txWeakerOpponent
    'Normal opponent', // txNormalOpponent
    'Stronger opponent', // txStrongerOpponent
    'Promotion', // txPromotion
    'Knight', // txKnight
    'Bishop', // txBishop
    'Rook', // txRook
    'Queen', // txQueen

    'To move a piece, click on it and then click on the square you wish to move it to.', // txHelp
    'Pascal chess program by Roland Chastain.', // txAbout
    'Illegal move.', // txIllegalMove

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
