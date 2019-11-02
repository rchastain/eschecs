
unit Validator;

interface

uses
  RegExpr;

type
  TValidator = class
    public
      function ExpandEmptySquares(AExpr: TRegExpr): string;
      function IsFen(const AStr: string): boolean;
  end;

implementation

uses
  Classes, SysUtils;

function TValidator.ExpandEmptySquares(AExpr: TRegExpr): string;
begin
  result := '';
  with AExpr do
    result := StringOfChar('-', StrToInt(Match[0]));
end;

function TValidator.IsFen(const AStr: string): boolean;
const
  WHITEKING = 'K';
  BLACKKING = 'k';
  PIECES    = '^[1-8BKNPQRbknpqr]+$';
  ACTIVE    = '^[wb]$';
  CASTLING  = '^[KQkq]+$|^[A-Ha-h]+$|^\-$';
  ENPASSANT = '^[a-h][36]$|^\-$';
  HALFMOVE  = '^\d+$';
  FULLMOVE  = '^[1-9]\d*$';
var
  a, b: TStrings;
  i: integer;
  e: TRegExpr;
  s: string;
  
begin
  a := TStringList.Create;
  b := TStringList.Create;
  
  e := TRegExpr.Create;
  e.Expression := '\d';
  
  (*
  ExtractStrings([' '], [], PChar(AStr), a);
  ExtractStrings(['/'], [], PChar(a[0]), b);
  *)
  
  SplitRegExpr(' ', AStr, a);
  
  result := (a.Count = 6);

  if result then
  begin
    SplitRegExpr('/', a[0], b);
    result := (b.Count = 8);
  end;
  
  if result then
  begin
    result := result and ExecRegExpr(WHITEKING, a[0]);
    result := result and ExecRegExpr(BLACKKING, a[0]);

    for i := 0 to 7 do
    begin
      result := result and ExecRegExpr(PIECES, b[i]);
      if result then
      begin
        s := b[i];
        repeat
          s := e.Replace(s, @ExpandEmptySquares);
        until not ExecRegExpr('\d', s);
        (*
        ToLog(Format('%s %s', [{$I %LINE%}, s]));
        *)
        result := result and (Length(s) = 8);
      end;
    end;
    
    result := result and ExecRegExpr(ACTIVE,    a[1]);
    result := result and ExecRegExpr(CASTLING,  a[2]);
    result := result and ExecRegExpr(ENPASSANT, a[3]);
    result := result and ExecRegExpr(HALFMOVE,  a[4]);
    result := result and ExecRegExpr(FULLMOVE,  a[5]);
  end;

  a.Free;
  b.Free;
  e.Free;
end;

(*
initialization
  loglist := TStringList.Create;
  logfile := ChangeFileExt({$I %FILE%}, '.log');

finalization
  with loglist do
  begin
    SaveToFile(logfile);
    Free;
  end;
*)

end.
