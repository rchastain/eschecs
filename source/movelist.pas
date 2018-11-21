
unit movelist;

{$H+}
{$ASSERTIONS ON}

interface

function CreateSample(const AMoveCount: integer): string;

type
  TMoveList = class
  private
    FArray: array of string;
    procedure FillArray(const ALine: string);
  public
    constructor Create(const ALine: string);
    destructor Destroy; override;
    function GetCount(): integer;
    procedure Append(const aMove: string); overload;
    procedure Append(const aMove: string; const aPreserve: integer); overload;
    procedure Clear;
    function GetString(const AMoveCount: integer = MAXINT): string;
  end;

implementation

uses
  SysUtils, StrUtils;

function CreateSample(const AMoveCount: integer): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to AMoveCount do
    result := Concat(
      result,
      Chr(Ord('a') + Random(8)),
      Chr(Ord('1') + Random(8)),
      Chr(Ord('a') + Random(8)),
      Chr(Ord('1') + Random(8)),
      IfThen(
        Random(4) = 0,
        Copy('nbrq', Succ(Random(4)), 1),
        ''
      )
    );
end;

constructor TMoveList.Create(const ALine: string);
begin
  SetLength(FArray, 0);
  FillArray(ALine);
end;

destructor TMoveList.Destroy;
begin
  Clear;
end;

function TMoveList.GetCount(): integer;
begin
  result := Length(FArray);
end;

procedure TMoveList.Append(const aMove: string);
begin
  SetLength(FArray, Succ(Length(FArray)));
  FArray[High(FArray)] := aMove;
end;

procedure TMoveList.Append(const aMove: string; const aPreserve: integer);
begin
  Assert(aPreserve <= GetCount());
  if aPreserve < GetCount() then
    SetLength(FArray, aPreserve);
  Append(aMove);
end;

procedure TMoveList.Clear;
begin
  SetLength(FArray, 0);
end;

function TMoveList.GetString(const AMoveCount: integer): string;
var
  i, j: integer;
begin
  result := '';
  j := GetCount();
  if AMoveCount < j then
    j := AMoveCount;
  for i := 0 to Pred(j) do
    result := Concat(result, FArray[i]);
end;

procedure TMoveList.FillArray(const ALine: string);
var
  vCharIndex, vMoveCount: integer;
  
  procedure Cut(const ALength: integer);
  const
    INCREMENT = 100;
  begin
    Inc(vMoveCount);
    if Length(FArray) < vMoveCount then SetLength(FArray, Length(FArray) + INCREMENT);
    FArray[Pred(vMoveCount)] := Copy(ALine, vCharIndex, ALength);
    Inc(vCharIndex, ALength);
  end;
  
begin
  vCharIndex := 1;
  vMoveCount := 0;
  while vCharIndex <= Length(ALine) - 3 do
  begin
    if  (ALine[vCharIndex + 0] in ['a'..'h'])
    and (ALine[vCharIndex + 1] in ['1'..'8'])
    and (ALine[vCharIndex + 2] in ['a'..'h'])
    and (ALine[vCharIndex + 3] in ['1'..'8']) then
    begin
      if vCharIndex = Length(ALine) - 3 then
        Cut(4)
      else if (vCharIndex = Length(ALine) - 4) then
        Cut(5)
      else if (ALine[vCharIndex + 4] in ['n', 'b', 'r', 'q']) and (ALine[vCharIndex + 5] in ['a'..'h']) then
        Cut(5)
      else
        Cut(4);
    end else
      Break;
  end;
  SetLength(FArray, vMoveCount);
end;

end.
