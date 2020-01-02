
unit language;

interface

uses
  SysUtils;
  
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
    txNew960,
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
    txVolume,
    txEnabled,

    txAboutMessage,
    txIllegalMove,
        
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
  
  TLanguage = (lgCzech, lgDutch, lgEnglish, lgFrench, lgGerman, lgItalian, lgSpanish);

function GetText(const AText: TText): string;

var
  LLang: TLanguage;
  
implementation

const
  CText: array[TLanguage, TText] of string = ((
    (* lgCzech *)
    'Eschecs', // txEschecs
    'Uložit', // txSave
    'Konec', // txQuit
    'O aplikaci Eschecs', // txAbout
    'Tahy', // txMoves
    'Počítač', // txComputerMove
    'Odpovědět automaticky', // txAutoPlay
    'Šachovnice', // txBoard
    'Nová hra', // txNewGame
    'Nová hra chess 960', // txNew960
    'Otočit šachovnici', // txFlip
    'Nastavení', // txOptions
    'Barva dlaždic', // txColoring
    'Zvuk', // txSound
    'Proměna', // txPromotion
    'Jezdec', // txKnight
    'Střelec', // txBishop
    'Věž', // txRook
    'Dáma', // txQueen

    'Zpráva', // txTitleMessage
    'Styl', // txStyle
    'Jazyk', // txLanguage
    'Hlasitost', // txVolume
    'Povoleno', // txEnabled

    'Šachový program v Pascalu od Rolanda Chastaina', // txAbout
    'Neplatný tah.', // txIllegalMove

    'Bílý na tahu.', // txWhiteToMove
    'Černý na tahu.', // txBlackToMove
    'Bílý vyhrál.', // txWhiteWins
    'Černý vyhrál.', // txBlackWins
    'Šach!', // txCheck
    'Mat!', // txCheckmate
    'Pat!', // txStalemate
    'Remíza!', // txDraw

    'Prosím počkejte...', // txWaiting
    'UCI protokol přijat.'#10'Název motoru: %s'#10'Autor: %s', // txUciOk
    'Nemohu se připojit k motoru.' // txConnectionFailure
  ), (
    (* lgDutch *)
    'Eschecs', // txEschecs
    'Redden', // txSave
    'Verlaten', // txQuit
    'Over Eschecs', // txAbout
    'Zetten', // txMoves
    'Computerzet', // txComputerMove
    'Automatisch antwoord', // txAutoPlay
    'Schaakbord', // txBoard
    'Nieuwe partij', // txNewGame
    'Nieuwe partij schaak 960', // txNew960
    'Het schaakbord draaien', // txFlip
    'Opties', // txOptions
    'Kleur van het schaakbord', // txColoring
    'Geluid', // txSound
    'Promotie', // txPromotion
    'Paard', // txKnight
    'Loper', // txBishop
    'Toren', // txRook
    'Koningin', // txQueen
    
    'Bericht', // txTitleMessage
    'Stijl', // txStyle
    'Taal', // txLanguage
    'Volume', // txVolume
    'Actief', // txEnabled

    'Schaakspel in Pascal geschreven door Roland Chastain', // txAbout
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
  ), (
    (* lgEnglish *)
    'Eschecs', // txEschecs
    'Save', // txSave
    'Quit', // txQuit
    'About Eschecs', // txAbout
    'Moves', // txMoves
    'Computer move', // txComputerMove
    'Autoplay', // txAutoPlay
    'Board', // txBoard
    'New game', // txNewGame
    'New chess 960 game', // txNew960
    'Flip board', // txFlip
    'Options', // txOptions
    'Square coloring', // txColoring
    'Sound', // txSound
    'Promotion', // txPromotion
    'Knight', // txKnight
    'Bishop', // txBishop
    'Rook', // txRook
    'Queen', // txQueen

    'Message', // txTitleMessage
    'Style', // txStyle
    'Language', // txLanguage
    'Volume', // txVolume
    'Enabled', // txEnabled

    'Pascal chess program by Roland Chastain', // txAbout
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
  ), (
    (* lgFrench *)
    'Eschecs', // txEschecs
    'Sauver', // txSave
    'Quitter', // txQuit
    'À propos d''Eschecs', // txAbout
    'Coups', // txMoves
    'Coup de l''ordinateur', // txComputerMove
    'Réponse automatique', // txAutoPlay
    'Échiquier', // txBoard
    'Nouvelle partie', // txNewGame
    'Nouvelle partie d''échecs 960', // txNew960
    'Tourner l''échiquier', // txFlip
    'Options', // txOptions
    'Coloriage des cases', // txColoring
    'Son', // txSound
    'Promotion', // txPromotion
    'Cavalier', // txKnight
    'Fou', // txBishop
    'Tour', // txRook
    'Dame', // txQueen

    'Message', // txTitleMessage
    'Style',  // txStyle
    'Langue', // txLanguage
    'Volume ', // txVolume
    'Activé', // txEnabled

    'Programme d''échecs en Pascal par Roland Chastain', // txAbout
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
  ), (
    (* lgGerman *)
    'Eschecs', // txEschecs
    'Speichern', // txSave
    'Ende', // txQuit
    'Über Eschecs', // txAbout
    'Züge', // txMoves
    'Ziehen!', // txComputerMove
    'Autoantwort', // txAutoPlay
    'Brett', // txBoard
    'Neue Partie', // txNewGame
    'Neue Partie Schach 960', // txNew960
    'Drehen', // txFlip
    'Optionen', // txOptions
    'Färbung des Schachbretts', // txColoring
    'Ton', // txSound
    'Umwandlung', // txPromotion
    'König', // txKnight
    'Läufer', // txBishop
    'Turm', // txRook
    'Dame', // txQueen
 
    'Nachricht', // txTitleMessage
    'Stil', // txStyle
    'Sprache', // txLanguage
    'Volume', // txVolume
    'Enabled', // txEnabled
  
    'Pascal Schachprogramm von Roland Chastain', // txAbout
    'Ungültiger Zug.', // txIllegalMove
    
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
    (* lgItalian *)
    'Eschecs', // txEschecs
    'Salva', // txSave
    'Esci', // txQuit
    'Informazioni su Eschecs', // txAbout
    'Movimento', // txMoves
    'Movimento Computer', // txComputerMove
    'Autoplay', // txAutoPlay
    'Scacchiera', // txBoard
    'Nuova partita', // txNewGame
    'Nuova partita scacchi 960', // txNew960
    'Inverti scacchiera', // txFlip
    'Opzioni', // txOptions
    'Colori', // txColoring
    'Suoni', // txSound
    'Promozione', // txPromotion
    'Cavallo', // txKnight
    'Alfiere', // txBishop
    'Torre', // txRook
    'Regina', // txQueen
 
    'Messaggio', // txTitleMessage
    'Stile', // txStyle
    'Lingua', // txLanguage
    'Volume', // txVolume
    'Abilitato', // txEnabled
 
    'Programma scacchi in Pascal di Roland Chastain', // txAbout
    'Mossa non valida', // txIllegalMove
 
    'Mossa al bianco.', // txWhiteToMove
    'Mossa al nero.', // txBlackToMove
    'Vince il bianco.', // txWhiteWins
    'Vince il nero.', // txBlackWins
    'Scacco!', // txCheck
    'Scaccomatto!', // txCheckmate
    'Stallo!', // txStalemate
    'Pareggio!', // txDraw
 
    'Attendere...', // txWaiting
    'Accettato protocollo UCI.'#10'Nome Engine: %s'#10'Autore: %s', // txUciOk
    'Impossibile collegarsi all''engine.' // txConnectionFailure
  ), (
    (* lgSpanish *)
    'Eschecs', // txEschecs
    'Guardar', // txSave
    'Salir', // txQuit
    'Acerca de Eschecs', // txAbout
    'Movimientos', // txMoves
    'Movimiento de la computadora', // txComputerMove
    'Partida automática', // txAutoPlay
    'Tablero', // txBoard
    'Nuevo juego', // txNewGame
    'Nuevo juego ajedrez 960', // txNew960
    'Invertir el tablero', // txFlip
    'Opciones', // txOptions
    'Colorear el tablero de ajedrez', // txColoring
    'Sonar', // txSound
    'Promover pieza', // txPromotion
    'Caballo', // txKnight
    'Alfil', // txBishop
    'Torre', // txRook
    'Reina', // txQueen

    'Mensaje', //txTitleMessage
    'Estilo',  //txStyle
    'Lengua', // txLanguage
    'Volumen', //txVolume
    'Activo',//txEnabled

    'Programa de Ajedrez en Pascal por Roland Chastain', // txAbout
    'Movimiento ilegal.', // txIllegalMove
    
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

function GetText(const AText: TText): string;
begin
  result := CText[LLang, AText];
end;

end.
