
{**
@abstract(Fonctions utilisées par l'unité @italic(chessposition).)
}
unit ChessUtils;

interface

function ComputeTargetSquare(const x1, y1, aVectorIndex: integer; out x2, y2: integer): boolean;
function SquareToStr(const x, y: integer): string;
function MoveToStr(const x1, y1, x2, y2: integer): string;
procedure StrToSquare(const aSquare: string; out x, y: integer);
procedure StrToMove(const aMove: string; out x1, y1, x2, y2: integer);

implementation

type
  TVector = record
    x, y: integer;
  end;
  
const
  VECTORS: array[1..16] of TVector = (
    (x:-1; y: 1),
    (x: 1; y: 1),
    (x:-1; y:-1),
    (x: 1; y:-1),
    (x:-1; y: 0),
    (x: 1; y: 0),
    (x: 0; y: 1),
    (x: 0; y:-1),
    (x: 1; y: 2),
    (x: 2; y: 1),
    (x: 2; y:-1),
    (x: 1; y:-2),
    (x:-1; y:-2),
    (x:-2; y:-1),
    (x:-2; y: 1),
    (x:-1; y: 2)
  );
  
  LITERALS: array[1..8, 1..8] of string[2] = (
    ('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8'),
    ('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8'),
    ('c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8'),
    ('d1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8'),
    ('e1', 'e2', 'e3', 'e4', 'e5', 'e6', 'e7', 'e8'),
    ('f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8'),
    ('g1', 'g2', 'g3', 'g4', 'g5', 'g6', 'g7', 'g8'),
    ('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8')
  );
  
function ComputeTargetSquare(const x1, y1, aVectorIndex: integer; out x2, y2: integer): boolean;
begin
  x2 := x1 + VECTORS[aVectorIndex].x;
  y2 := y1 + VECTORS[aVectorIndex].y;
  
  result := (x2 >= 1) and (x2 <= 8) and (y2 >= 1) and (y2 <= 8);
end;

function SquareToStr(const x, y: integer): string;
begin
  result := LITERALS[x, y];
end;

function MoveToStr(const x1, y1, x2, y2: integer): string;
begin
  result := Concat(LITERALS[x1, y1], LITERALS[x2, y2]);
end;

procedure StrToSquare(const aSquare: string; out x, y: integer);
begin
  Assert(Length(aSquare) = 2);
  x := Ord(aSquare[1]) - Ord('a') + 1;
  y := Ord(aSquare[2]) - Ord('1') + 1;
end;

procedure StrToMove(const aMove: string; out x1, y1, x2, y2: integer);
begin
  if Length(aMove) >= 4 then
  begin
    x1 := Ord(aMove[1]) - Ord('a') + 1;
    y1 := Ord(aMove[2]) - Ord('1') + 1;
    x2 := Ord(aMove[3]) - Ord('a') + 1;
    y2 := Ord(aMove[4]) - Ord('1') + 1;
  end else
  begin
    x1 := 1;
    y1 := 1;
    x2 := 1;
    y2 := 1;
  end;
end;

end.
