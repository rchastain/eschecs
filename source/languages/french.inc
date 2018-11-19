
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Aide', // txHelp
    'Sauver', // txSave
    'Quitter', // txQuit
    'À propos d''Eschecs', // txAbout
    'Coups', // txMoves
    'Coup de l''ordinateur', // txComputerMove
    'Réponse automatique', // txAutoPlay
    'Échiquier', // txBoard
    'Nouvelle partie', // txNewGame
    'Tourner l''échiquier', // txFlip
    'Options', // txOptions
    'Coloriage des cases', // txColoring
    'Son', // txSound
    'Promotion', // txPromotion
    'Cavalier', // txKnight
    'Fou', // txBishop
    'Tour', // txRook
    'Dame', // txQueen

    'Message',           //txTitleMessage
    'Pour déplacer une pièce, cliquez sur la case de départ puis sur la case d''arrivée.', // txHelp
    'Programme d''échecs en Pascal par Roland Chastain.', // txAbout
    'Coup illégal.', // txIllegalMove
    'Le changement de style sera effectif au prochain lancement de l''application.', // txStyleInfo
    
    'Blancs au trait.', // txWhiteToMove
    'Noirs au trait.', // txBlackToMove
    'Les blancs gagnent.', // txWhiteWins
    'Les noirs gagnent.', // txBlackWins
    'Échec !', // txCheck
    'Échec et mat !', // txCheckmate
    'Pat !', // txStalemate
    'Partie remise !', // txDraw
    
    'Veuillez patienter...', // txWaiting
    'Protocole UCI accepté.'#10'Nom du moteur : %s'#10'Auteur : %s', // txUciOk
    'Impossible d''établir une connexion avec le moteur.' // txConnectionFailure
  );

  STYLENAME: array[TStyle] of string = (
    'Échiquier 320 Original',
    'Échiquier 480 Simple',
    'Échiquier 480 Marbre',
    'Échiquier 480 Nouveau',
    'Échiquier 640 Bois'
  );
