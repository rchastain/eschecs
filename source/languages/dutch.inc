
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Hulp', // txHelp
    'Verlaten', // txQuit
    'Over Eschecs', // txAbout
    'Zetten', // txMoves
    'Computerzet', // txComputerMove
    'Automatisch antwoord', // txAutoPlay
    'Schaakbord', // txBoard
    'Nieuwe partij', // txNewGame
    'Het schaakbord draaien', // txFlip
    'Opties', // txOptions
    'Square coloring [to be translated]', // txColoring
    'Sound [to be translated]', // txSound
    'Zwakkere tegenstander', // txWeakerOpponent
    'Normale tegenstander', // txNormalOpponent
    'Sterkere tegenstander', // txStrongerOpponent
    'Promotie', // txPromotion
    'Paard', // txKnight
    'Loper', // txBishop
    'Toren', // txRook
    'Koningin', // txQueen

    'Om een speelstuk te verplaatsen, klik erop en dan op het vakje waarop je het wil plaatsen.', // txHelp
    'Schaakspel in Pascal geschreven door Roland Chastain.', // txAbout
    'Illegale beweging.', // txIllegalMove

    'Wit aan zet.', // txWhiteToMove
    'Zwart aan zet.', // txBlackToMove
    'Wit wint.', // txWhiteWins
    'Zwart wint.', // txBlackWins
    'Schaak!', // txCheck
    'Schaakmat!', // txCheckmate
    'Pat!', // txStalemate
    'Remise!', // txDraw
    
    'Even geduld aub...', // txWaiting
    'UCI protocol aanvaard.'#10'Schaakengine: %s'#10'Schepper: %s', // txUciOk
    'Kan niet verbinden met de engine.' // txConnectionFailure
  );
