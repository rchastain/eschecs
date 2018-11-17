
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Ayuda', // txHelp
    'Salir', // txQuit
    'Acerca de Eschecs', // txAbout
    'Movimientos', // txMoves
    'Movimiento de la computadora', // txComputerMove
    'Partida automática', // txAutoPlay
    'Tablero', // txBoard
    'Nuevo juego', // txNewGame
    'Invertir el tablero', // txFlip
    'Opciones', // txOptions
    'Square coloring [to be translated]', // txColoring
    'Sound [to be translated]', // txSound
    'Promover pieza', // txPromotion
    'Caballo', // txKnight
    'Alfil', // txBishop
    'Torre', // txRook
    'Reina', // txQueen

    'Para mover una pieza, haga click en ella y después haga click en el escaque al que quiera moverla.', // txHelp
    'Programa de Ajedrez en Pascal por Roland Chastain.', // txAbout
    'Movimiento ilegal.', // txIllegalMove
    'Le changement de style sera effectif au prochain lancement de l''application. [à traduire]', // txStyleInfo
    
    'Blancas mueven.', // txWhiteToMove
    'Negras mueven.', // txBlackToMove
    'Blancas ganan.', // txWhiteWins
    'Negras ganan.', // txBlackWins
    '¡Jaque!', // txCheck
    '¡Jaque mate!', // txCheckmate
    '¡Ahogado!', // txStalemate
    '¡Tablas!', // txDraw
    
    'Espere un momento, por favor...', // txWaiting
    'Protocolo UCI aceptado.'#10'Nombre del motor: %s'#10'Autor: %s', // txUciOk
    'No pudo conectarse al motor.' // txConnectionFailure
  );
