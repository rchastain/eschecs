
unit language;

interface

uses
  SysUtils,
  Style;
  
type
  TText = (
    txEschecs,
    txHelp,
    txSave,
    txQuit,
    txAbout,
    txMoves,
    txComputerMove,
    txAutoPlay,
    txBoard,
    txNew,
    txFlip,
    txOptions,
    txColoring,
    txSound,
    txPromotion,
    txKnight,
    txBishop,
    txRook,
    txQueen,
    
    txTitleMessage,
    txStyle,
    txLanguage,

    txHelpMessage,
    txAboutMessage,
    txIllegalMove,
    txStyleInfo,
        
    txWhiteToMove,
    txBlackToMove,
    txWhiteWins,
    txBlackWins,
    txCheck,
    txCheckmate,
    txStalemate,
    txDraw,
    
    txWaiting,
    txUciOk,
    txConnectionFailure
  );
  
  TLanguage = (lgDutch, lgEnglish, lgFrench, lgGerman, lgSpanish);

function GetText(const aText: TText): string;
function GetStyleName(const aStyle: TStyle): string;

var
  gLanguage: TLanguage;
  
implementation

const
  TEXTS: array[TLanguage, TText] of string = ((
    // lgDutch
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
    
    'Bericht', //txTitleMessage
    'Estilo',  //txStyle
    'Taal', // txLanguage

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
  ), (
    // lgEnglish
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

    'Message', //txTitleMessage
    'Style',  //txStyle
    'Language', // txLanguage

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
  ), (
    // lgFrench
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

    'Message', //txTitleMessage
    'Style',  //txStyle
    'Langue', // txLanguage

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
  ), (
    // lgGerman
    'Eschecs', // txEschecs
    'Hilfe', // txHelp
    'Speichern', // txSave
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
 
    'Nachricht',           //txTitleMessage
    'Stil',  //txStyle
    'Sprache', // txLanguage
  
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
  ), (
    // lgSpanish
    'Eschecs', // txEschecs
    'Ayuda', // txHelp
    'Guardar', // txSave
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

    'Mensaje', //txTitleMessage
    'Estilo',  //txStyle
    'Lengua', // txLanguage

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
  ));
  
  STYLENAME: array[TLanguage, TStyle] of string = ((
    // lgDutch
    'Schaakbord 320 Origineel',
    'Schaakbord 480 Eenvoudig',
    'Schaakbord 480 Marmer',
    'Schaakbord 480 Nieuw',
    'Schaakbord 640 Hout'
  ), (
    // lgEnglish
    'Chessboard 320 Original',
    'Chessboard 480 Simple',
    'Chessboard 480 Marble',
    'Chessboard 480 New',
    'Chessboard 640 Wood'
  ), (
    // lgFrench
    'Échiquier 320 Original',
    'Échiquier 480 Simple',
    'Échiquier 480 Marbre',
    'Échiquier 480 Nouveau',
    'Échiquier 640 Bois'
  ), (
    // lgGerman
    'Schachbrett 320 Original',
    'Schachbrett 480 Einfach',
    'Schachbrett 480 Marmor',
    'Schachbrett 480 Neu',
    'Schachbrett 640 Holz'
  ), (
    // lgSpanish
    'Tablero 320 Original',
    'Tablero 480 Sencillo',
    'Tablero 480 Mármol',
    'Tablero 480 Nuevo',
    'Tablero 640 Madera'
  ));

function GetText(const aText: TText): string;
begin
  result := TEXTS[gLanguage, aText];
end;

function GetStyleName(const aStyle: TStyle): string;
begin
  result := STYLENAME[gLanguage, aStyle];
end;

end.
