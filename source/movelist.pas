
unit MoveList;

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
    function GetCount: integer;
    procedure Append(const AMove: string); overload;
    procedure Append(const AMove: string; const APreserve: integer); overload;
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

function TMoveList.GetCount: integer;
begin
  result := Length(FArray);
end;

procedure TMoveList.Append(const AMove: string);
begin
  SetLength(FArray, Succ(Length(FArray)));
  FArray[High(FArray)] := AMove;
end;

procedure TMoveList.Append(const AMove: string; const APreserve: integer);
begin
  Assert(APreserve <= GetCount);
  if APreserve < GetCount then
    SetLength(FArray, APreserve);
  Append(AMove);
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
  j := GetCount;
  if AMoveCount < j then
    j := AMoveCount;
  for i := 0 to Pred(j) do
    result := Concat(result, FArray[i]);
end;

procedure TMoveList.FillArray(const ALine: string);
var
  LCharIndex, LMoveCount: integer;
  
  procedure Cut(const ALength: integer);
  const
    CInc = 100;
  begin
    Inc(LMoveCount);
    if Length(FArray) < LMoveCount then SetLength(FArray, Length(FArray) + CInc);
    FArray[Pred(LMoveCount)] := Copy(ALine, LCharIndex, ALength);
    Inc(LCharIndex, ALength);
  end;
  
begin
  LCharIndex := 1;
  LMoveCount := 0;
  while LCharIndex <= Length(ALine) - 3 do
  begin
    if  (ALine[LCharIndex + 0] in ['a'..'h'])
    and (ALine[LCharIndex + 1] in ['1'..'8'])
    and (ALine[LCharIndex + 2] in ['a'..'h'])
    and (ALine[LCharIndex + 3] in ['1'..'8']) then
    begin
      if LCharIndex = Length(ALine) - 3 then
        Cut(4)
      else if (LCharIndex = Length(ALine) - 4) then
        Cut(5)
      else if (ALine[LCharIndex + 4] in ['n', 'b', 'r', 'q']) and (ALine[LCharIndex + 5] in ['a'..'h']) then
        Cut(5)
      else
        Cut(4);
    end else
      Break;
  end;
  SetLength(FArray, LMoveCount);
end;

end.
