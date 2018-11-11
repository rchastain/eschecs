
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Aide', // txHelp
    'Quitter', // txQuit
    'À propos d''Eschecs', // txAbout
    'Coups', // txMoves
    'Coup de l''ordinateur', // txComputerMove
    'Réponse automatique', // txAutoPlay
    'Échiquier', // txBoard
    'Nouvelle partie', // txNewGame
    'Tourner l''échiquier', // txFlip
    'Options', // txOptions
    'Échiquier de marbre', // txMarble
    'Son', // txSound
    'Adversaire plus faible', // txWeakerOpponent
    'Adversaire normal', // txNormalOpponent
    'Adversaire plus fort', // txStrongerOpponent
    'Promotion', // txPromotion
    'Cavalier', // txKnight
    'Fou', // txBishop
    'Tour', // txRook
    'Dame', // txQueen

    'Pour déplacer une pièce, cliquez sur la case de départ puis sur la case d''arrivée.', // txHelp
    'Programme d''échecs en Pascal par Roland Chastain.', // txAbout
    'Coup illégal.', // txIllegalMove

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
