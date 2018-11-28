
unit Schach;

interface
  
type
  TChessboard = array[-10..109] of integer;

const
  cBlack  =  -1;
  cNil    =   0;
  cWhite  =   1;
  cPawn   =   2;
  cBishop =   6;
  cKnight =   7;
  cRook   =  10;
  cQueen  =  19;
  cKing   = 126;
  cBorder = 127;

type
  TCastling = (g1, c1, g8, c8);
  
  TEvaluatedMove = record
    a, b, v: integer;
  end;
  
  TMoveList = array[1..100] of TEvaluatedMove;

  TChessPosition = class
    strict private
      board: TChessboard;
      active: integer;
      castling: array[TCastling] of boolean;
      enPassant: array[cBlack..cWhite] of integer;
      moveList: TMoveList;
      count: integer;
      balance: integer;
      twoKings: boolean;
    public
      constructor Create(const aFENRecord: string); overload;
      procedure SetPositionFromFENRecord(const aFENRecord: string);
      procedure SetPositionFromPosition(const aPosition: TChessPosition);
      function Evaluation(const aColor: integer): integer;
      procedure MovePiece(a, b: integer; const aPromotion: integer);
      procedure AppendMove(const a, b: integer);
      procedure GenerateMoves;
      function Check(): boolean;
      function CastlingCheck(const aKingSquare, aStep: integer): boolean;
      procedure GenerateCastling(const aKingSquare, aStep, aRookSquare: integer);
      procedure GenerateAllMoves;
      function RecursiveEvaluation(const aColor, aDepth, aAlpha: integer): integer;
      function IsLegal(const aMove: integer): boolean;
      function BoardAsText(): string;
      function FENRecord(): string;
      property activeColor: integer read active write active;
      property moveCount: integer read count;
      property firstMoveList: TMoveList read moveList;
      property chessboard: TChessboard read board;
  end;
  
  TBestMoveError = (errSuccess, errCheck, errCheckmate, errStalemate, errMoveNotFound);
  
  TJSChess = class
    strict private
      initialPos,
      currentPos: TChessPosition;
      halfmoves: integer;
      secondMoveList: TMoveList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetPosition(const aFENRecord: string);
      function PlayMove(const aMove: string): boolean;
      function BestMoveIndex(const aRecursiveEvaluationResult: integer): integer;
      function BestMove(out aErr: TBestMoveError): string; overload;
      function BestMove(): string; overload;
      procedure SetSearchDepth(const aMinDepth, aMaxDepth: integer);
      function CurrentBoardAsText(const aPretty: boolean = true): string;
      function FENRecord(): string;
  end;

var
  gReturn: boolean;

implementation

uses
  Classes, SysUtils, TypInfo;

var
  minDepth: integer = 3;
  maxDepth: integer = 5;
  
const
  mPawn:   array[1..4] of integer = ( 01,  02, -09,  11);
  mBishop: array[1..4] of integer = ( 11, -11,  09, -09);
  mRook:   array[1..4] of integer = (-01,  01, -10,  10);
  mKnight: array[1..8] of integer = ( 12,  21,  19,  08, -12, -21, -19, -08);
  mKing:   array[1..8] of integer = (-01,  01, -10,  10,  11, -11,  09, -09);
  
  conventional_start_position = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

function LetterToInt(const c: char): integer;
begin
  result := Ord(c) - Ord('a') + 1;
end;

function DigitToInt(const c: char): integer;
begin
  result := Ord(c) - Ord('1') + 1;
end;

function DigitToLetter(const c: char): char;
begin
  result := Chr(Ord(c) - Ord('1') + Ord('a'));
end;

function LetterToDigit(const c: char): char;
begin
  result := Chr(Ord(c) - Ord('a') + Ord('1'));
end;

function MoveToStr(const a, b: integer): string;
begin
  result := IntToStr(100 * a + b);
  result[1] := DigitToLetter(result[1]);
  result[3] := DigitToLetter(result[3]);
end;

function MoveToInt(aMove: string): integer;
begin
  aMove[1] := LetterToDigit(aMove[1]);
  aMove[3] := LetterToDigit(aMove[3]);
  result := StrToInt(aMove);
end;

function ComputeHalfmoves(const aFENRecord: string): integer;
var
  fields: TStringList;
begin
  fields := TStringList.Create;
  ExtractStrings([' '], [], pchar(aFENRecord), fields);
  result := 2 * (StrToInt(fields[5]) - 1);
  if fields[1] = 'b' then
    Inc(result);
  fields.Free;
end;

function PieceToInt(const aPiece: char): integer;
begin
  case aPiece of
    'P': result := cWhite * cPawn;
    'N': result := cWhite * cKnight;
    'B': result := cWhite * cBishop;
    'R': result := cWhite * cRook;
    'Q': result := cWhite * cQueen;
    'K': result := cWhite * cKing;
    'p': result := cBlack * cPawn;
    'n': result := cBlack * cKnight;
    'b': result := cBlack * cBishop;
    'r': result := cBlack * cRook;
    'q': result := cBlack * cQueen;
    'k': result := cBlack * cKing;
    else
      result := cNil;
  end;
end;

function PieceToChar(const aPiece: integer): char;
begin
  case aPiece of
    cWhite * cPawn:   result := 'P';
    cWhite * cKnight: result := 'N';
    cWhite * cBishop: result := 'B';
    cWhite * cRook:   result := 'R';
    cWhite * cQueen:  result := 'Q';
    cWhite * cKing:   result := 'K';
    cNil:             result := '.';
    cBlack * cPawn:   result := 'p';
    cBlack * cKnight: result := 'n';
    cBlack * cBishop: result := 'b';
    cBlack * cRook:   result := 'r';
    cBlack * cQueen:  result := 'q';
    cBlack * cKing:   result := 'k';
    cBorder:          result := '#'
    else
      result := '?';
  end;
end;

constructor TChessPosition.Create(const aFENRecord: string);
begin
  inherited Create;
  SetPositionFromFENRecord(aFENRecord);
end;

procedure TChessPosition.SetPositionFromFENRecord(const aFENRecord: string);
var
  fields: TStringList;
  x, y, i, j: integer;
begin
  fields := TStringList.Create;
  ExtractStrings([' '], [], pchar(aFENRecord), fields);
  Assert(fields.Count = 6);

  for i := Low(board) to High(board) do
    board[i] := cBorder;
  
  x := 1;
  y := 8;
  for i := 1 to Length(fields[0]) do
  begin
    case fields[0][i] of
      '/':
        begin
          x := 1;
          Dec(y);
        end;
      '1'..'8':
        begin
          j := DigitToInt(fields[0][i]);
          while j > 0 do
          begin
            board[10 * x + y] := cNil;
            Inc(x);
            Dec(j);
          end;
        end;
      'P', 'N', 'B', 'R', 'Q', 'K', 'p', 'n', 'b', 'r', 'q', 'k':
        begin
          board[10 * x + y] := PieceToInt(fields[0][i]);
          Inc(x);
        end;
    end;
  end;

  if fields[1] = 'w' then active := cWhite else active := cBlack;

  castling[g1] := Pos('K', fields[2]) > 0;
  castling[c1] := Pos('Q', fields[2]) > 0;
  castling[g8] := Pos('k', fields[2]) > 0;
  castling[c8] := Pos('q', fields[2]) > 0;

  if fields[3] <> '-' then
    enPassant[active] := 10 * LetterToInt(fields[3][1]) + DigitToInt(fields[3][2]);
  enPassant[cBlack * active] := cNil;
  
  balance := 0;
  for x := 1 to 8 do
    for y := 1 to 8 do
      Inc(
        balance,
        board[10 * x + y]
      );
  
  twoKings := (Pos('K', fields[0]) > 0) and (Pos('k', fields[0]) > 0);
  
  fields.Free;
end;

procedure TChessPosition.SetPositionFromPosition(const aPosition: TChessPosition);
begin
  board := aPosition.board;
  active := aPosition.active;
  castling := aPosition.castling;
  enPassant := aPosition.enPassant;
  moveList := aPosition.moveList;
  count := aPosition.count;
  balance := aPosition.balance;
  twoKings := aPosition.twoKings;
end;

function TChessPosition.Evaluation(const aColor: integer): integer;
begin
  result := balance * aColor;
end;

procedure TChessPosition.MovePiece(a, b: integer; const aPromotion: integer);
begin
  if board[a] * active = cKing then
    case a of
      51:
        begin
          castling[g1] := false;
          castling[c1] := false;
        end;
      58:
        begin
          castling[g8] := false;
          castling[c8] := false;
        end;
    end;

  if board[a] * active = cRook then
    case a of
      81: castling[g1] := false;
      11: castling[c1] := false;
      88: castling[g8] := false;
      18: castling[c8] := false;
    end;

  if ((b - a) * (b - a) = 4) and (board[a] * active = cPawn) then
    enPassant[active] := a + mPawn[1] * active
  else
    enPassant[active] := cNil;

  if (b = enPassant[cBlack * active]) and (board[a] * active = cPawn) then
  begin
    enPassant[active] := cNil;
    MovePiece(a, (b div 10) * 10 + a mod 10, cQueen);
    active := cBlack * active;
    a := (b div 10) * 10 + a mod 10;
  end;
  
  if (a in [51, 58]) and (b in [71, 31, 78, 38]) and (board[a] * active = cKing) then
  begin
    board[b] := board[a];
    board[a] := cNil;
    if b div 10 = 7 then
    begin
      a := a mod 10 + 80;
      b := a - 20;
    end else
    begin
      a := a mod 10 + 10;
      b := a + 30;
    end;
    board[b] := board[a];
    board[a] := cNil;
  end else
  begin
    balance := balance - board[b];
    
    if board[b] * cBlack * active = cKing then
      twoKings := false;
    
    board[b] := board[a];
    board[a] := cNil;
    if ((b mod 10 = 1) or (b mod 10 = 8)) and (board[b] * active = cPawn) then
    begin
      board[b] := aPromotion * active;
      balance := balance + (aPromotion - cPawn) * active;
    end;
  end;

  active := cBlack * active;
end;

procedure TChessPosition.AppendMove(const a, b: integer);
begin
  if not twoKings then
    exit;
  Inc(count);
  moveList[count].a := a;
  moveList[count].b := b;
end;

procedure TChessPosition.GenerateMoves;
var
  a, b,
  x, y,
  i, j: integer;
begin
  count := 0;
  for x := 1 to 8 do
    for y := 1 to 8 do
    begin
      a := 10 * x + y;
      if board[a] <> cNil then
      begin
        case board[a] * active of
          cPawn:
            begin
              b := a + mPawn[1] * active;
              if board[b] = cNil then
              begin
                AppendMove(a, b);
                if (active = cWhite) and (y = 2) or (active = cBlack) and (y = 7) then
                begin
                  b := a + mPawn[2] * active;
                  if (board[b] = cNil) then
                    AppendMove(a, b);
                end;
              end;
              for i := 3 to 4 do
              begin
                b := a + mPawn[i] * active;
                if (cBlack * board[b] * active in [cPawn..cKing]) or (b = enPassant[cBlack * active]) then
                  AppendMove(a, b);
              end;
            end;
          cKnight:
            for i := 1 to 8 do
            begin
              b := a + mKnight[i];
              if cBlack * board[b] * active in [cNil..cKing] then
                AppendMove(a, b);
            end;
          cBishop:
            for i := 1 to 4 do
            begin
              b := a;
              repeat
                Inc(b, mBishop[i]);
                if cBlack * board[b] * active in [cNil..cKing] then
                  AppendMove(a, b);
              until board[b] <> cNil;
            end;
          cRook:
            for i := 1 to 4 do
            begin
              b := a;
              repeat
                Inc(b, mRook[i]);
                if cBlack * board[b] * active in [cNil..cKing] then
                  AppendMove(a, b);
              until board[b] <> 0;
            end;
          cKing:
            for i := 1 to 8 do
            begin
              b := a + mKing[i];
              if cBlack * board[b] * active in [cNil..cKing] then
              begin
                j := 0;
                repeat
                  Inc(j);
                until (cBlack * board[b + mKing[j]] * active = cKing) or (j = 8);
                if j = 8 then
                  AppendMove(a, b);
              end;
            end;
          cQueen:
            for i := 1 to 8 do
            begin
              b := a;
              repeat
                Inc(b, mKing[i]);
                if cBlack * board[b] * active in [cNil..cKing] then
                  AppendMove(a, b);
              until board[b] <> cNil;
            end;
        end;
      end;
    end;
end;

function TChessPosition.Check(): boolean;
var
  index: integer;
begin
  result := false;
  active := cBlack * active;

  GenerateMoves;
  active := cBlack * active;

  for index := 1 to count do
    if board[moveList[index].b] * active = cKing then
    begin
      result := true;
      exit;
    end;
end;

function TChessPosition.CastlingCheck(const aKingSquare, aStep: integer): boolean;
var
  index: integer;
begin
  result := false;
  active := cBlack * active;

  GenerateMoves;
  active := cBlack * active;

  for index := 1 to count do
    if (moveList[index].b mod 10 = aKingSquare mod 10) and ((moveList[index].b - aKingSquare) div aStep >= 0) then
    begin
      result := true;
      exit;
    end;
end;

procedure TChessPosition.GenerateCastling(const aKingSquare, aStep, aRookSquare: integer);
var
  aux: TChessPosition;
  square: integer;
begin
  if board[aRookSquare] * active <> cRook then
    exit;
  square := aKingSquare + aStep;
  repeat
    if board[square] <> cNil then
      exit;
    Inc(square, aStep);
  until square = aRookSquare;
  
  aux := TChessPosition.Create;
  aux.SetPositionFromPosition(self);

  if not aux.CastlingCheck(aKingSquare, aStep) then
    AppendMove(aKingSquare, aKingSquare + 2 * aStep);

  aux.Free;
end;

procedure TChessPosition.GenerateAllMoves;
begin
  GenerateMoves;
  
  if active = cWhite then
  begin
    if castling[g1] then GenerateCastling(51, +10, 81);
    if castling[c1] then GenerateCastling(51, -10, 11);
  end else
  begin
    if castling[g8] then GenerateCastling(58, +10, 88);
    if castling[c8] then GenerateCastling(58, -10, 18);
  end;
end;

function TChessPosition.RecursiveEvaluation(const aColor, aDepth, aAlpha: integer): integer;
var
  eval,
  index,
  beta: integer;
  aux: TChessPosition;
  stop: boolean;
begin
  GenerateAllMoves;

  index := 0;
  beta := -32000 * active * aColor;

  aux := TChessPosition.Create;

  stop := false;
  while (index < count) and not stop do
  begin
    Inc(index);

    aux.SetPositionFromPosition(self);

    aux.MovePiece(aux.moveList[index].a, aux.moveList[index].b, cQueen);

    if (aDepth >= minDepth) and (board[moveList[index].b] = cNil) or (aDepth = maxDepth) or gReturn then
      eval := aux.Evaluation(aColor)
    else
      eval := aux.RecursiveEvaluation(aColor, aDepth + 1, beta);

    if active = aColor then
    begin
      if eval > beta then
        beta := eval;
      if beta > aAlpha then
        stop := true;
    end else
    begin
      if eval < beta then
        beta := eval;
      if beta < aAlpha then
        stop := true;
    end;

    moveList[index].v := eval;
  end;
  result := beta;

  aux.Free;
end;

function TChessPosition.IsLegal(const aMove: integer): boolean;
var
  i: integer;
begin
  result := false;
  i := Low(TMoveList);
  while (i <= count) and not result do
    if aMove = 100 * moveList[i].a + moveList[i].b then
      result := true
    else
      Inc(i);
end;

function TChessPosition.BoardAsText(): string;
var
  i: integer;
begin
  SetLength(result, 120);
  for i := 1 to 120 do
    result[i] := PieceToChar(board[i - 11]);
end;

function TChessPosition.FENRecord(): string;
var
  x, y, n: integer;
  act, castl, pass: string;
begin
  result := '';
  x := 1;
  y := 8;
  while y >= 1 do
  begin
    if board[10 * x + y] = cNil then
    begin
      n := 0;
      while (x + n <= 8) and (board[10 * (x + n) + y] = cNil) do
        Inc(n);
      result := Concat(result, IntToStr(n));
      Inc(x, n);
    end else
    begin
      result := Concat(result, PieceToChar(board[10 * x + y]));
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
  
  if active = cWhite then
    act := 'w'
  else
    act := 'b';
  
  castl := '';
  if castling[g1] then castl := Concat(castl, 'K');
  if castling[c1] then castl := Concat(castl, 'Q');
  if castling[g8] then castl := Concat(castl, 'k');
  if castling[c8] then castl := Concat(castl, 'q');
  if castl = '' then
    castl := '-';
  
  if enPassant[active] = cNil then
    pass := '-'
  else
  begin
    pass := IntToStr(enPassant[active]);
    pass[1] := DigitToLetter(pass[1]);
  end;
  
  result := Format(
    '%s %s %s %s %d %d',
    [
      result,
      act,
      castl,
      pass,
      0,
      1
    ]
  );
end;

constructor TJSChess.Create;
begin
  inherited Create;
  initialPos := TChessPosition.Create(conventional_start_position);
  currentPos := TChessPosition.Create;
end;

destructor TJSChess.Destroy;
begin
  currentPos.Free;
  initialPos.Free;
  inherited Destroy;
end;

procedure TJSChess.SetPosition(const aFENRecord: string);
begin
  currentPos.SetPositionFromFENRecord(aFENRecord);
  halfmoves := ComputeHalfmoves(aFENRecord);
end;

function TJSChess.PlayMove(const aMove: string): boolean;
var
  aux: TChessPosition;
  i,
  a, b: integer;
begin
  i := MoveToInt(aMove);
  a := i div 100;
  b := i mod 100;
  currentPos.GenerateAllMoves;
  result := currentPos.IsLegal(i);
  if result then
  begin
    aux := TChessPosition.Create;
    aux.SetPositionFromPosition(currentPos);
    aux.MovePiece(a, b, cQueen);
    aux.activeColor := cBlack * aux.activeColor;
    if aux.Check then
      result := false
    else
    begin
      currentPos.MovePiece(a, b, cQueen);
      Inc(halfmoves);
    end;
    aux.Free;
  end;
end;

function TJSChess.BestMoveIndex(const aRecursiveEvaluationResult: integer): integer;
var
  aux: TChessPosition;
  i, j: integer;
  maxValue: integer;
  secondListCount: integer;
begin
  aux := TChessPosition.Create;

  with currentPos do
  begin
    secondListCount := 0;
    for i := 1 to moveCount do
      if firstMoveList[i].v = aRecursiveEvaluationResult then
      begin
        Inc(secondListCount);
        secondMoveList[secondListCount].a := firstMoveList[i].a;
        secondMoveList[secondListCount].b := firstMoveList[i].b;
        secondMoveList[secondListCount].v := 0
      end;
  end;

  maxValue := Low(integer);
  result := 0;
  
  for i := 1 to secondListCount do
  begin
    aux.SetPositionFromPosition(currentPos);

    with aux do
    begin
      if chessboard[secondMoveList[i].a] = initialPos.chessboard[secondMoveList[i].a] then
      begin
        Inc(secondMoveList[i].v, 5);
        if chessboard[secondMoveList[i].a] * activeColor = cPawn then
          Inc(secondMoveList[i].v, 2);
      end;

      if chessboard[secondMoveList[i].a] * activeColor = cKing then
        Dec(secondMoveList[i].v, 10);

      if (halfmoves < 32) and (chessboard[secondMoveList[i].a] * activeColor in [cPawn, cBishop, cKnight]) then
        Inc(secondMoveList[i].v, 20);

      if (secondMoveList[i].a div 10 = 1)
        or (secondMoveList[i].a div 10 = 8)
        or (secondMoveList[i].a mod 10 = 1)
        or (secondMoveList[i].a mod 10 = 8) then
        Inc(secondMoveList[i].v, 2);

      if (secondMoveList[i].b div 10 = 1)
        or (secondMoveList[i].b div 10 = 8)
        or (secondMoveList[i].b mod 10 = 1)
        or (secondMoveList[i].b mod 10 = 8) then
        Dec(secondMoveList[i].v, 2);
    end;

    aux.SetPositionFromPosition(currentPos);

    aux.MovePiece(secondMoveList[i].a, secondMoveList[i].b, cQueen);

    if aux.chessboard[secondMoveList[i].b] = initialPos.chessboard[secondMoveList[i].b] then
      Dec(secondMoveList[i].v, 10);

    aux.activeColor := cBlack * aux.activeColor;
    aux.GenerateMoves;

    with aux do
      for j := 1 to moveCount do
      begin
        Inc(secondMoveList[i].v);
        if chessboard[firstMoveList[j].b] <> cNil then
          Inc(secondMoveList[i].v);
      end;

    aux.activeColor := cBlack * aux.activeColor;
    aux.GenerateMoves;

    with aux do
      for j := 1 to moveCount do
      begin
        Dec(secondMoveList[i].v);
        if chessboard[firstMoveList[j].b] <> cNil then
          Dec(secondMoveList[i].v);
      end;

    if secondMoveList[i].v >= maxValue then
    begin
      maxValue := secondMoveList[i].v;
      result := i;
    end;
  end;

  aux.Free;
end;
  
function TJSChess.BestMove(out aErr: TBestMoveError): string;
var
  moveIndex: integer;
  before: boolean;
  eval: integer;
begin
  result := '0000';
  gReturn := false;
  
  eval := currentPos.RecursiveEvaluation(currentPos.activeColor, 1, 32000);
  moveIndex := BestMoveIndex(eval);
  
  if moveIndex = 0 then
  begin
    aErr := errMoveNotFound; // Error (cannot find any move)
    exit;
  end;
  
  Inc(halfmoves);
  before := currentPos.Check;
  currentPos.MovePiece(
    secondMoveList[moveIndex].a,
    secondMoveList[moveIndex].b,
    cQueen
  );
  currentPos.activeColor := cBlack * currentPos.activeColor;
  if currentPos.Check then
    if before then
      aErr := errCheckmate
    else
      aErr := errStalemate
  else
  begin
    result := MoveToStr(
      secondMoveList[moveIndex].a,
      secondMoveList[moveIndex].b
    );
    currentPos.activeColor := cBlack * currentPos.activeColor;
    if currentPos.Check then
      aErr := errCheck
    else
      aErr := errSuccess;
  end;
end;

function TJSChess.BestMove(): string;
var
  _: TBestMoveError;
begin
  result := BestMove(_);
end;

procedure TJSChess.SetSearchDepth(const aMinDepth, aMaxDepth: integer);
begin
  minDepth := aMinDepth;
  maxDepth := aMaxDepth;
end;

function TJSChess.CurrentBoardAsText(const aPretty: boolean): string;
var
  vText, vLine: string;
  x, y: integer;
begin
  vText := currentPos.BoardAsText();
  if aPretty then
  begin
    result :=          '+  a b c d e f g h  +'#10;
    result := result + '                     '#10;
    for y := 8 downto 1 do
    begin
      vLine := Format('%d                   %d', [y, y]);
      for x := 1 to 8 do
        vLine[ 2 * x + 2] := vText[10 * x + y + 11];
      result := result + vLine + #10;
    end;
    result := result + '                     '#10;
    result := result + '+  a b c d e f g h  +';
  end else
    result := vText;
end;

function TJSChess.FENRecord(): string;
begin
  result := currentPos.FENRecord();
end;

end.
