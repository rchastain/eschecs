
unit language;

interface

uses
  SysUtils,
  Style;
  
type
  TText = (
    txEschecs,
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

    txAboutMessage,
    txIllegalMove,
    txChangeSaved,
        
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
function GetLanguageName(const aLanguage: TLanguage): string;

var
  gLanguage: TLanguage;
  
implementation

const
  TEXTS: array[TLanguage, TText] of string = ((
    // lgDutch
    'Eschecs', // txEschecs
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
    '', // txColoring
    'Geluid', // txSound
    'Promotie', // txPromotion
    'Paard', // txKnight
    'Loper', // txBishop
    'Toren', // txRook
    'Koningin', // txQueen
    
    'Bericht', //txTitleMessage
    'Estilo',  //txStyle
    'Taal', // txLanguage

    'Schaakspel in Pascal geschreven door Roland Chastain.', // txAbout
    'Illegale beweging.', // txIllegalMove
    'The change will be effective at the next launch of the application.', // txChangeSaved

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

    'Pascal chess program by Roland Chastain.', // txAbout
    'Illegal move.', // txIllegalMove
    'The change will be effective at the next launch of the application.', // txChangeSaved

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

    'Programme d''échecs en Pascal par Roland Chastain.', // txAbout
    'Coup illégal.', // txIllegalMove
    'La modification sera effective au prochain lancement de l''application.', // txChangeSaved
    
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
    '', // txColoring
    'Ton', // txSound
    'Umwandlung', // txPromotion
    'König', // txKnight
    'Läufer', // txBishop
    'Turm', // txRook
    'Dame', // txQueen
 
    'Nachricht',           //txTitleMessage
    'Stil',  //txStyle
    'Sprache', // txLanguage
  
    'Pascal Schachprogramm von Roland Chastain.', // txAbout
    'Ungültiger Zug.', // txIllegalMove
    'The change will be effective at the next launch of the application.', // txChangeSaved
    
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
    '', // txColoring
    'Sonar', // txSound
    'Promover pieza', // txPromotion
    'Caballo', // txKnight
    'Alfil', // txBishop
    'Torre', // txRook
    'Reina', // txQueen

    'Mensaje', //txTitleMessage
    'Estilo',  //txStyle
    'Lengua', // txLanguage

    'Programa de Ajedrez en Pascal por Roland Chastain.', // txAbout
    'Movimiento ilegal.', // txIllegalMove
    'The change will be effective at the next launch of the application.', // txChangeSaved
    
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

function GetLanguageName(const aLanguage: TLanguage): string;
const
  S: array[TLanguage] of string = (
    'Dutch',
    'English',
    'French',
    'German',
    'Spanish'
  );
begin
  result := S[aLanguage];
end;

end.
