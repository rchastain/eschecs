
{**
@abstract(Fonctions utilisées par l'unité @italic(chessposition).)
}
unit ChessUtils;

interface

function TargetSquare(const x1, y1, i: integer; out x2, y2: integer): boolean;
function SquareToStr(const x, y: integer): string;
function MoveToStr(const x1, y1, x2, y2: integer): string;
procedure StrToSquare(const ASquare: string; out x, y: integer);
procedure StrToMove(const AMove: string; out x1, y1, x2, y2: integer);

implementation

type
  TVector = record
    x, y: integer;
  end;
  
const
  CVectors: array[1..16] of TVector = (
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
  
  CSquareNames: array[1..8, 1..8] of string[2] = (
    ('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8'),
    ('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8'),
    ('c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8'),
    ('d1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8'),
    ('e1', 'e2', 'e3', 'e4', 'e5', 'e6', 'e7', 'e8'),
    ('f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8'),
    ('g1', 'g2', 'g3', 'g4', 'g5', 'g6', 'g7', 'g8'),
    ('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8')
  );
  
function TargetSquare(const x1, y1, i: integer; out x2, y2: integer): boolean;
begin
  Assert((i >= Low(CVectors)) and (i <= High(CVectors)));
  
  x2 := x1 + CVectors[i].x;
  y2 := y1 + CVectors[i].y;
  
  result := (x2 >= 1) and (x2 <= 8) and (y2 >= 1) and (y2 <= 8);
end;

function SquareToStr(const x, y: integer): string;
begin
  result := CSquareNames[x, y];
end;

function MoveToStr(const x1, y1, x2, y2: integer): string;
begin
  result := Concat(CSquareNames[x1, y1], CSquareNames[x2, y2]);
end;

procedure StrToSquare(const ASquare: string; out x, y: integer);
begin
  Assert(Length(ASquare) = 2);
  x := Ord(ASquare[1]) - Ord('a') + 1;
  y := Ord(ASquare[2]) - Ord('1') + 1;
end;

procedure StrToMove(const AMove: string; out x1, y1, x2, y2: integer);
begin
  Assert(Length(AMove) >= 4);
  if Length(AMove) >= 4 then
  begin
    x1 := Ord(AMove[1]) - Ord('a') + 1;
    y1 := Ord(AMove[2]) - Ord('1') + 1;
    x2 := Ord(AMove[3]) - Ord('a') + 1;
    y2 := Ord(AMove[4]) - Ord('1') + 1;
  end;
end;

end.
