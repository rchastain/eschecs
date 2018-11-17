
unit language;

interface

uses
  SysUtils,
  StrUtils,
  ChessTypes;
  
type
  TText = (
    txEschecs,
    txHelp,
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
    txWeakerOpponent,
    txNormalOpponent,
    txStrongerOpponent,
    txPromotion,
    txKnight,
    txBishop,
    txRook,
    txQueen,
    
    txHelpMessage,
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

function ArbitratorMessage(const aCheck: boolean; const aActiveColor: TChessPieceColor; const aState: TChessState): string;

{$include language.inc}

implementation

function ArbitratorMessage(const aCheck: boolean; const aActiveColor: TChessPieceColor; const aState: TChessState): string;
begin
  case aState of
    csProgress:
        result := Concat(
          IfThen(aCheck, Concat(TEXTS[txCheck], ' '), ''),
          IfThen(aActiveColor = cpcWhite, TEXTS[txWhiteToMove], TEXTS[txBlackToMove])
        );
    csCheckmate:
        result := Concat(
          TEXTS[txCheckmate], ' ',
          IfThen(aActiveColor = cpcWhite, TEXTS[txBlackWins], TEXTS[txWhiteWins])
        );
    csStalemate:
      result := TEXTS[txStalemate];
    csDraw:
      result := TEXTS[txDraw];
  end;
end;

end.
