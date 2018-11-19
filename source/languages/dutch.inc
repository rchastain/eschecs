
const
  TEXTS: array[TText] of string = (
    'Eschecs', // txEschecs
    'Hulp', // txHelp
    'Redden', // txSave
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
    'Promotie', // txPromotion
    'Paard', // txKnight
    'Loper', // txBishop
    'Toren', // txRook
    'Koningin', // txQueen
    
    'Bericht',           //txTitleMessage
    'Om een speelstuk te verplaatsen, klik erop en dan op het vakje waarop je het wil plaatsen.', // txHelp
    'Schaakspel in Pascal geschreven door Roland Chastain.', // txAbout
    'Illegale beweging.', // txIllegalMove
    'Le changement de style sera effectif au prochain lancement de l''application. [à traduire]', // txStyleInfo

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

  STYLENAME: array[TStyle] of string = (
    'Schaakbord 320 Origineel',
    'Schaakbord 480 Eenvoudig',
    'Schaakbord 480 Marmer',
    'Schaakbord 480 Nieuw',
    'Schaakbord 640 Hout'
  );
