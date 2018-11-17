
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Hilfe', // txHelp
    'Ende', // txQuit
    'Über Eschecs', // txAbout
    'Züge', // txMoves
    'Ziehen!', // txComputerMove
    'Autoantwort', // txAutoPlay
    'Brett', // txBoard
    'Neue Partie', // txNewGame
    'Drehen', // txFlip
    'Optionen', // txOptions
    'Square coloring [to be translated]', // txColoring
    'Sound [to be translated]', // txSound
    'Umwandlung', // txPromotion
    'König', // txKnight
    'Läufer', // txBishop
    'Turm', // txRook
    'Dame', // txQueen

    'Um eine Figur zu bewegen, klicken Sie zuerst auf die Figur und dann auf das Zielfeld.', // txHelp
    'Pascal Schachprogramm von Roland Chastain.', // txAbout
    'Ungültiger Zug.', // txIllegalMove
    'Le changement de style sera effectif au prochain lancement de l''application. [à traduire]', // txStyleInfo
    
    'Weiß am Zug.', // txWhiteToMove
    'Schwarz am Zug.', // txBlackToMove
    'Weiß gewinnt.', // txWhiteWins
    'Schwarz gewinnt.', // txBlackWins
    'Schach!', // txCheck
    'Schachmatt!', // txCheckmate
    'Patt!', // txStalemate
    'Remis!', // txDraw
    
    'Bitte warten.', // txWaiting
    'UCI Protokoll akzeptiert.'#10'Engine Name: %s'#10'Programmierer: %s', // txUciOk
    'Verbindung mit Engine nicht möglich.' // txConnectionFailure
  );
