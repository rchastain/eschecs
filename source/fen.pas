
{**
@abstract(Lecture et écriture des chaînes @html(<a href="http://kirill-kryukov.com/chess/doc/fen.html">FEN</a>).)
}
unit Fen;

interface

uses
  ChessTypes;

const
  CFenStartPosition = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
  CFenStartPosition518 = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1';

function EncodePositionData(const AFenRecord: string): TPositionData;
function DecodePositionData(const AData: TPositionData; const AFrc: boolean): string;

implementation

uses
  SysUtils, Classes, RegExpr;

const
  CColorSymbol: array[TPieceColorStrict] of char = ('w', 'b');
  CTable = 'PNBRQK';

function XToChar(const x: integer; const a: char): char;
begin
  result := Chr(Ord(a) + Pred(x));
end;

procedure EncodeCastlingRights(const AFen: string; const ABoard: TBoard; const AWhiteKing, ABlackKing: TPoint; var ARights: TCastlingRights);
var
  LExpr: TRegExpr;
  x: integer;
begin
  LExpr := TRegExpr.Create;
  if (AWhiteKing.y = 1) and (AWhiteKing.x > 1) and (AWhiteKing.x < 8) then
  begin
    LExpr.Expression := Format('[%s-HK]', [XToChar(AWhiteKing.x, 'A')]);
    if LExpr.Exec(AFen) then
    begin
      x := 8;
      while x > AWhiteKing.x do
      begin
        if (ABoard[x, 1].FType = ptRook) and (ABoard[x, 1].FColor = pcWhite)
        and ((LExpr.Match[0] = XToChar(x, 'A')) or (LExpr.Match[0] = 'K')) then
          ARights[caWH] := x;
        Dec(x);
      end;
    end;
    LExpr.Expression := Format('[A-%sQ]', [XToChar(AWhiteKing.x, 'A')]);
    if LExpr.Exec(AFen) then
    begin
      x := 1;
      while x < AWhiteKing.x do
      begin
        if (ABoard[x, 1].FType = ptRook) and (ABoard[x, 1].FColor = pcWhite)
        and ((LExpr.Match[0] = XToChar(x, 'A')) or (LExpr.Match[0] = 'Q')) then
          ARights[caWA] := x;
        Inc(x);
      end;
    end;
  end;
  if (ABlackKing.y = 8) and (ABlackKing.x > 1) and (ABlackKing.x < 8) then
  begin
    LExpr.Expression := Format('[%s-hk]', [XToChar(ABlackKing.x, 'a')]);
    if LExpr.Exec(AFen) then
    begin
      x := 8;
      while x > ABlackKing.x do
      begin
        if (ABoard[x, 8].FType = ptRook) and (ABoard[x, 8].FColor = pcBlack)
        and ((LExpr.Match[0] = XToChar(x, 'a')) or (LExpr.Match[0] = 'k')) then
          ARights[caBH] := x;
        Dec(x);
      end;
    end;
    LExpr.Expression := Format('[a-%sq]', [XToChar(ABlackKing.x, 'a')]);
    if LExpr.Exec(AFen) then
    begin
      x := 1;
      while x < ABlackKing.x do
      begin
        if (ABoard[x, 8].FType = ptRook) and (ABoard[x, 8].FColor = pcBlack)
        and ((LExpr.Match[0] = XToChar(x, 'a')) or (LExpr.Match[0] = 'q')) then
          ARights[caBA] := x;
        Inc(x);
      end;
    end;
  end;
  LExpr.Free;
end;

function EncodePositionData(const AFenRecord: string): TPositionData;
var
  x, y, i: integer;
  c: char;
  LWhiteKing, LBlackKing: TPoint;
begin
  with TStringList.Create do
  begin
    DelimitedText := AFenRecord;
    Assert(Count = 6);
    with result do
    begin
      x := 1;
      y := 8;
      i := 1;
      while i <= Length(Strings[0]) do
      begin
        c := Strings[0][i];
        case c of
          '/':
            begin
              x := 1;
              Dec(y);
            end;
          '1'..'8':
            while c > '0' do
            begin
              FBoard[x, y].FColor := pcNil;
              FBoard[x, y].FType := ptNil;
              Inc(x);
              Dec(c);
            end;
        else
          begin
            FBoard[x, y].FColor := TPieceColorStrict(Ord(c in ['a'..'z']));
            FBoard[x, y].FType := TPieceTypeStrict(Pred(Pos(UpCase(c), CTable)));
            if c = 'K' then
              LWhiteKing.SetLocation(x, y)
            else
              if c = 'k' then
                LBlackKing.SetLocation(x, y);
            Inc(x);
          end;
        end;
        Inc(i);
      end;
      FActive := TPieceColorStrict(Ord(Strings[1] = CColorSymbol[pcBlack]));
      
      FCastling[caWH] := 0;
      FCastling[caWA] := 0;
      FCastling[caBH] := 0;
      FCastling[caBA] := 0;
      
      EncodeCastlingRights(Strings[2], FBoard, LWhiteKing, LBlackKing, FCastling);
      
      FEnPassant := Strings[3];
      FHalfMoves := StrToInt(Strings[4]);
      FFullMove := StrToInt(Strings[5]);
    end;
    Free;
  end;
end;

function DecodeCastlingRights(const ARights: TCastlingRights; const AFrc: boolean): string;
begin
  result := '';
  if AFrc then
  begin
    if ARights[caWH] <> 0 then result := result + XToChar(ARights[caWH], 'A');
    if ARights[caWA] <> 0 then result := result + XToChar(ARights[caWA], 'A');
    if ARights[caBH] <> 0 then result := result + XToChar(ARights[caBH], 'a');
    if ARights[caBA] <> 0 then result := result + XToChar(ARights[caBA], 'a');
  end else
  begin
    if ARights[caWH] <> 0 then result := result + 'K';
    if ARights[caWA] <> 0 then result := result + 'Q';
    if ARights[caBH] <> 0 then result := result + 'k';
    if ARights[caBA] <> 0 then result := result + 'q';
  end;
  if result = '' then
    result := '-';
end;

function DecodePositionData(const AData: TPositionData; const AFrc: boolean): string;
var
  x, y, n: integer;
begin
  with AData do
  begin
    result := '';
    x := 1;
    y := 8;
    while y >= 1 do
    begin
      if FBoard[x, y].FType = ptNil then
      begin
        n := 0;
        while (x + n <= 8) and (FBoard[x + n, y].FType = ptNil) do
          Inc(n);
        result := Concat(result, IntToStr(n));
        Inc(x, n);
      end else
      begin
        result := Concat(result, CPieceSymbol[FBoard[x, y].FColor, FBoard[x, y].FType]);
        Inc(x);
      end;
      if x > 8 then
      begin
        if y > 1 then
          result := Concat(result, '/');
        x := 1;
        Dec(y);
      end;
    end;
    
    result := Format(
      '%s %s %s %s %d %d',
      [
        result,
        CColorSymbol[FActive],
        DecodeCastlingRights(FCastling, AFrc),
        FEnPassant,
        FHalfMoves,
        FFullMove
      ]
    );
  end;
end;

end.
