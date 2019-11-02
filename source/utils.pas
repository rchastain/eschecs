
unit Utils;

interface

uses
  SysUtils;

procedure DecodeSquare(const aSquare: string; out x, y: integer);
procedure DecodeMove(const aMove: string; out x1, y1, x2, y2: integer);
function EncodeSquare(const x, y: integer): string;
function XToScreen(const x: integer; const aUpSideDown: boolean): integer;
function YToScreen(const y: integer; const aUpSideDown: boolean): integer;

var
  LConfigFilesPath: TFileName;
  LScale: integer;
  
implementation

procedure DecodeSquare(const aSquare: string; out x, y: integer);
begin
  Assert(Length(aSquare) = 2);
  x := Ord(aSquare[1]) - Ord('a') + 1;
  y := Ord(aSquare[2]) - Ord('1') + 1;
end;

procedure DecodeMove(const aMove: string; out x1, y1, x2, y2: integer);
begin
  Assert(Length(aMove) >= 4);
  x1 := Ord(aMove[1]) - Ord('a') + 1;
  y1 := Ord(aMove[2]) - Ord('1') + 1;
  x2 := Ord(aMove[3]) - Ord('a') + 1;
  y2 := Ord(aMove[4]) - Ord('1') + 1;
end;

function EncodeSquare(const x, y: integer): string;
begin
  result := Concat(
    Chr(Ord('a') + x - 1),
    Chr(Ord('1') + y - 1)
  );
end;

function XToScreen(const x: integer; const aUpSideDown: boolean): integer;
begin
  if aUpsideDown then
    result := 8 - x
  else
    result := x - 1;
  result := LScale * result;
end;

function YToScreen(const y: integer; const aUpSideDown: boolean): integer;
begin
  if aUpsideDown then
    result := y - 1
  else
    result := 8 - y;
  result := LScale * result;
end;

begin
  LConfigFilesPath := Concat(ExtractFilePath(ParamStr(0)), 'config', DirectorySeparator);
end.
