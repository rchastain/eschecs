
(*******************************************************************)
(*                                                                 *)
(*  MATER: Mate searching program - (c) Valentin Albillo 1998      *)
(*                                                                 *)
(*      This program or parts thereof can be used for any purpose  *)
(*  whatsoever as long as proper credit is given to the copyright  *)
(*  holder. Absolutely no guarantees given, no liabilities of any  *)
(*  kind accepted. Use at your own risk.  Your using this code in  *)
(*  all or in part does indicate your acceptance of these terms.   *)
(*                                                                 *)
(*******************************************************************)

unit Mater;

interface

function SolveMate(
  const aFen: string;
  const aMovesNumber: integer;
  const aSearchAllMoves: boolean
): string;

implementation

uses
  SysUtils, StrUtils;
  
const
  MAXMOVES = 200;
  MAXPIECES = 16;
  PAWN = 1;
  KNIGHT = 2;
  BISHOP = 3;
  ROOK = 4;
  QUEEN = 5;
  KING = 6;
  WHITE = 1;
  BLACK = -1;
  NONE = 0;
  TOP = 22;
  BOTTOM = 99;
  ANY = 1;
  CAPTURE = -1;
  ENPASSANT = 8;
  OO = 6;
  OOO = 7;
  VOO = 50;
  VOOO = 30;

function Color(i: integer): integer;
begin
  if i = 0 then
    result := 0
  else if i < 0 then
    result := BLACK
  else
    result := WHITE;
end;

type
  TSetOfSquare = set of 1..120;
  TArrayOfBoolean = array[BLACK..WHITE] of boolean;
  TArrayOfSquare = array[BLACK..WHITE] of TSetOfSquare;

  TPositionRecord = record
    board: array[1..120] of integer;
    kingCastle: TArrayOfBoolean;
    queenRookCastle: TArrayOfBoolean;
    kingRookCastle: TArrayOfBoolean;
    enPassantSquare: integer;
  end;

  TMoveRecord = record
    sqFrom, sqTo, moveClass, moveVal: integer;
  end;

  TArrayOfMove = array[1..MAXMOVES] of TMoveRecord;
  TArrayOfPiece = array[1..MAXPIECES] of integer;

  TAuxiliaryData = array[BLACK..WHITE] of record
    kingSquare: integer;
    piecesNumber: integer;
    pieces: TArrayOfPiece;
  end;

const
  VP = 100;
  VN = 300;
  VB = 300;
  VR = 500;
  VQ = 900;
  VK = 9999;

  VALUE: array[BLACK * KING..WHITE * KING] of integer = (
    VK, VQ, VR, VB, VN, VP, 0,
    VP, VN, VB, VR, VQ, VK
  );

  DIRPAWN  : array[1..3] of integer = (10, 9, 11);
  DIRKNIGHT: array[1..8] of integer = (-21, -19, -12, -8,  8, 12, 19, 21);
  DIRBISHOP: array[1..4] of integer = (-11, -9, 9, 11);
  DIRROOK  : array[1..4] of integer = (-10, -1, 1, 10);
  DIRQUEEN : array[1..8] of integer = (-11, -10, -9, -1, 1, 9, 10, 11);
  DIRKING  : array[1..8] of integer = (-11, -10, -9, -1, 1, 9, 10, 11);

  SQPROMO: TArrayOfSquare = ([BOTTOM - 7..BOTTOM], [], [TOP..TOP + 7]);
  SQPRIME: TArrayOfSquare = ([32..39], [], [82..89]);
  SQENPASSANTCAPTURE: TArrayOfSquare = ([72..79], [], [42..49]);

  SQQUEENROOKS: array[BLACK..WHITE] of integer = (TOP, NONE, BOTTOM - 7);
  SQKINGROOKS : array[BLACK..WHITE] of integer = (TOP + 7, NONE, BOTTOM);

var
  gPosition: TPositionRecord;
  gNodes: integer;

function SquareToStr(aSquare: integer): string;
begin
  result := Concat(
    Chr(aSquare mod 10 - 2 + Ord('a')),
    Chr(9 - aSquare div 10 + Ord('1'))
  );
end;

function StrToSquare(aStr: string): integer;
begin
  result := 10 * (Ord(aStr[1]) - Ord('a') + 2) + Ord(aStr[2]) - Ord('1') + 2;
end;

function InitializeGlobalPosition(aFen: string; var aTurn: integer): boolean;
var
  a: array[1..6] of string;
  x, y, i, j: integer;
begin
  for i := 1 to 6 do a[i] := ExtractWord(i, aFen, [' ']);

  x := 1;
  y := 8;
  i := 1;

  with gPosition do
  begin
    while i <= Length(a[1]) do
    begin
      case UpCase(a[1][i]) of
        'P', 'N', 'B', 'R', 'Q', 'K':
          begin
            case a[1][i] of
              'p': board[10 * (10 - y) + x + 1] := BLACK * PAWN;
              'n': board[10 * (10 - y) + x + 1] := BLACK * KNIGHT;
              'b': board[10 * (10 - y) + x + 1] := BLACK * BISHOP;
              'r': board[10 * (10 - y) + x + 1] := BLACK * ROOK;
              'q': board[10 * (10 - y) + x + 1] := BLACK * QUEEN;
              'k': board[10 * (10 - y) + x + 1] := BLACK * KING;
              'P': board[10 * (10 - y) + x + 1] := WHITE * PAWN;
              'N': board[10 * (10 - y) + x + 1] := WHITE * KNIGHT;
              'B': board[10 * (10 - y) + x + 1] := WHITE * BISHOP;
              'R': board[10 * (10 - y) + x + 1] := WHITE * ROOK;
              'Q': board[10 * (10 - y) + x + 1] := WHITE * QUEEN;
              'K': board[10 * (10 - y) + x + 1] := WHITE * KING;
            end;
            Inc(x);
          end;
        '1'..'8':
          begin
            j := Ord(aFen[i]) - Ord('0');
            while j > 0 do
            begin
              board[10 * (10 - y) + x + 1] := 0;
              Inc(x);
              Dec(j);
            end;
          end;
        '/':
          begin
            x := 1;
            Dec(y);
          end;
        else
          begin
            result := FALSE;
            exit;
          end;
      end;
      Inc(i);
    end;
    
    queenRookCastle[BLACK] := (Pos('q', a[3]) > 0);
    kingRookCastle[BLACK] := (Pos('k', a[3]) > 0);
    kingCastle[BLACK] := queenRookCastle[BLACK] or kingRookCastle[BLACK];

    queenRookCastle[WHITE] := (Pos('Q', a[3]) > 0);
    kingRookCastle[WHITE] := (Pos('K', a[3]) > 0);
    kingCastle[WHITE] := queenRookCastle[WHITE] or kingRookCastle[WHITE];

    if a[4] = '-' then
      enPassantSquare := NONE
    else
      enPassantSquare := StrToSquare(a[4]);
  end;
  
  if a[2] = 'w' then
    aTurn := WHITE
  else if a[2] = 'b' then
    aTurn := BLACK
  else
  begin
    result := FALSE;
    exit;
  end;
  
  result := TRUE;
end;

procedure FillAuxiliaryData(var aData: TAuxiliaryData);
var
  i: integer;
begin
  FillChar(aData, SizeOf(aData), 0);
  with gPosition do
    for i := TOP to BOTTOM do
      if Abs(board[i]) in [PAWN..KING] then
        with aData[Color(board[i])] do
        begin
          Inc(piecesNumber);
          pieces[piecesNumber] := i;
          if board[i] = Color(board[i]) * KING then
            kingSquare := i;
        end;
end;

function InCheck(aColor, aKingSquare, aOtherKingSquare: integer): boolean;
var
  i, s, b, d: integer;
begin
  result := TRUE;

  if Abs(aKingSquare - aOtherKingSquare) in [1, 9..11] then
    exit;

  with gPosition do
  begin
    for i := 1 to 4 do
    begin
      d := DIRBISHOP[i];
      s := aKingSquare;
      repeat
        Inc(s, d);
        b := board[s];
      until b <> 0;
      if (b = -1 * aColor * BISHOP) or (b = -1 * aColor * QUEEN) then
          exit;

      d := DIRROOK[i];
      s := aKingSquare;
      repeat
        Inc(s, d);
        b := board[s];
      until b <> 0;
      if (b = -1 * aColor * ROOK) or (b = -1 * aColor * QUEEN) then
          exit;
    end;

    for i := 1 to 8 do
      if board[aKingSquare + DIRKNIGHT[i]] = -1 * aColor * KNIGHT then
        exit;

    for i := 2 to 3 do
      if board[aKingSquare + -1 * aColor * DIRPAWN[i]] = -1 * aColor * PAWN then
        exit;
  end;
  result := FALSE;
end;

procedure GenerateMoves(
  aColor: integer;
  aSquare: integer;
  var aMoves: TArrayOfMove;
  var aMovesCount: integer;
  aKingSquare, aOtherKingSquare: integer;
  aLegal: boolean;
  aSingle: boolean;
  var aFound: boolean
);

var
  s, b, i, d: integer;
  r: TPositionRecord;
  v, c: integer;

  procedure TestRecordMove(aBoard, aClass, aValue: integer);
  begin
    if aLegal then
    begin
      r := gPosition;
      with gPosition do
      begin
        board[s] := aBoard;
        board[aSquare] := 0;
        if aClass = -1 * ENPASSANT then
          board[s + DIRPAWN[1] * aColor] := 0;
        if InCheck(aColor, aKingSquare, aOtherKingSquare) then
        begin
          gPosition := r;
          exit;
        end;
        if aSingle then
        begin
          aFound := TRUE;
          gPosition := r;
          exit;
        end;
      end;
      gPosition := r;
    end;
    Inc(aMovesCount);
    with aMoves[aMovesCount] do
    begin
      sqFrom := aSquare;
      sqTo := s;
      moveClass := aClass;
      moveVal := aValue;
    end;
  end;

  procedure TestRecordPawn;
  begin
    v := VALUE[Abs(b)];

    if v = 0 then
      c := ANY
    else
      c := CAPTURE;

    if s in SQPROMO[aColor] then
    begin
      TestRecordMove(aColor * QUEEN, QUEEN * c, v + VQ);
      if aFound then
        exit;
      TestRecordMove(aColor * ROOK, ROOK * c, v + VR);
      if aFound then
        exit;
      TestRecordMove(aColor * BISHOP, BISHOP * c, v + VB);
      if aFound then
        exit;
      TestRecordMove(aColor * KNIGHT, KNIGHT * c, v + VN);
      if aFound then
        exit;
    end else
    begin
      TestRecordMove(PAWN, c, v);
      if aFound then
        exit;
    end;
  end;

  procedure TestCastling;
  var
    i: integer;
  label
    sig;
  begin
    with gPosition do
    begin
      if not kingCastle[aColor] then
        exit;
      aKingSquare := aSquare;
      if kingRookCastle[aColor] then
      begin
        for i := Succ(aKingSquare) to aKingSquare + 2 do
          if board[i] <> 0 then
            goto sig;
        if InCheck(aColor, aKingSquare, aOtherKingSquare) then
          exit;
        for i := Succ(aKingSquare) to aKingSquare + 2 do
          if InCheck(aColor, i, aOtherKingSquare) then
            goto sig;
        if aSingle then
        begin
          aFound := TRUE;
          exit;
        end;
        Inc(aMovesCount);
        with aMoves[aMovesCount] do
        begin
          sqFrom := aSquare;
          sqTo := aKingSquare + 2;
          moveClass := OO;
          moveVal := VOO;
        end;
      end;
      sig:
      if queenRookCastle[aColor] then
      begin
        for i := aKingSquare - 3 to Pred(aKingSquare) do
          if board[i] <> 0 then
            exit;
        if InCheck(aColor, aKingSquare, aOtherKingSquare) then
          exit;
        for i := aKingSquare - 2 to Pred(aKingSquare) do
          if InCheck(aColor, i, aOtherKingSquare) then
            exit;
        if aSingle then
        begin
          aFound := TRUE;
          exit;
        end;
        Inc(aMovesCount);
        with aMoves[aMovesCount] do
        begin
          sqFrom := aSquare;
          sqTo := aKingSquare - 2;
          moveClass := OOO;
          moveVal := VOOO;
        end;
      end;
    end;
  end;

begin
  aFound := FALSE;
  Inc(gNodes);
  with gPosition do
  begin
    aMovesCount := 0;
    case Abs(board[aSquare]) of
      PAWN:
        begin
          d := - 1 * aColor * DIRPAWN[1];
          s := aSquare + d;
          b := board[s];
          if b = 0 then
          begin
            TestRecordPawn;
            if aFound then
              exit;
            if aSquare in SQPRIME[aColor] then
            begin
              Inc(s, d);
              b := board[s];
              if b = 0 then
              begin
                TestRecordPawn;
                if aFound then
                  exit;
              end;
            end;
          end;
          for i := 2 to 3 do
          begin
            s := aSquare - 1 * aColor * DIRPAWN[i];
            if s = enPassantSquare then
            begin
              if s in SQENPASSANTCAPTURE[aColor] then
              begin
                TestRecordMove(PAWN, -ENPASSANT, VP);
                if aFound then
                  exit;
              end;
            end else
            begin
              b := board[s];
              if Abs(b) in [PAWN..KING] then
                if b * - 1 * aColor > 0 then
                begin
                 TestRecordPawn;
                 if aFound then
                   exit;
                end;
            end;
          end;
        end;
      KNIGHT:
        for i := 1 to 8 do
        begin
          s := aSquare + DIRKNIGHT[i];
          b := board[s];
          if b <> 7 then
            if b * aColor <= 0 then
            begin
              v := VALUE[Abs(b)];
              if v = 0 then
                c := ANY
              else
                c := CAPTURE;
              TestRecordMove(board[aSquare], c, v);
              if aFound then
                exit;
            end;
        end;
      BISHOP:
        for i := 1 to 4 do
        begin
          s := aSquare;
          repeat
            Inc(s, DIRBISHOP[i]);
            b := board[s];
            if b <> 7 then
              if b * aColor <= 0 then
              begin
                v := VALUE[Abs(b)];
                if v = 0 then
                  c := ANY
                else
                  c := CAPTURE;
                TestRecordMove(board[aSquare], c, v);
                if aFound then
                  exit;
              end;
          until b <> 0;
        end;
      ROOK:
        for i := 1 to 4 do
        begin
          s := aSquare;
          repeat
            Inc(s, DIRROOK[i]);
            b := board[s];
            if b <> 7 then
              if b * aColor <= 0 then
              begin
                v := VALUE[Abs(b)];
                if v = 0 then
                  c := ANY
                else
                  c := CAPTURE;
                TestRecordMove(board[aSquare], c, v);
                if aFound then
                  exit;
              end;
          until b <> 0;
        end;
      QUEEN:
        for i := 1 to 8 do
        begin
          s := aSquare;
          repeat
            Inc(s, DIRQUEEN[i]);
            b := board[s];
            if b <> 7 then
              if b * aColor <= 0 then
              begin
                v := VALUE[Abs(b)];
                if v = 0 then
                  c := ANY
                else
                  c := CAPTURE;
                TestRecordMove(board[aSquare], c, v);
                if aFound then
                  exit;
              end;
          until b <> 0;
        end;
      KING:
        begin
          for i := 1 to 8 do
          begin
            s := aSquare + DIRKING[i];
            b := board[s];
            aKingSquare := s;
            if b <> 7 then
              if b * aColor <= 0 then
              begin
                v := VALUE[Abs(b)];
                if v = 0 then
                  c := ANY
                else
                  c := CAPTURE;
                TestRecordMove(board[aSquare], c, v);
                if aFound then
                  exit;
              end;
          end;
          TestCastling;
          if aFound then
            exit;
        end;
    end;
  end;
end;

function AnyMoveSide(
  aColor: integer;
  var aData: TAuxiliaryData;
  aKingSquare,
  aOtherKingSquare: integer
): boolean;
var
  i, lMovesNumber: integer;
  lMoves: TArrayOfMove;
  lFound: boolean;
begin
  with aData[aColor] do
  begin
    GenerateMoves(
      aColor,
      aKingSquare,
      lMoves,
      lMovesNumber,
      aKingSquare,
      aOtherKingSquare,
      TRUE,
      TRUE,
      lFound
    );
    if lFound then
    begin
      result := TRUE;
      exit;
    end;
    for i := 1 to piecesNumber do
      if pieces[i] <> aKingSquare then
      begin
        GenerateMoves(
          aColor,
          pieces[i],
          lMoves,
          lMovesNumber,
          aKingSquare,
          aOtherKingSquare,
          TRUE,
          TRUE,
          lFound
        );
        if lFound then
        begin
          result := TRUE;
          exit;
        end;
      end;
  end;
  result := FALSE;
end;

procedure PerformMove(
  var aMove: TMoveRecord;
  aColor: integer;
  var aKingSquare: integer
);
var
  b: integer;
begin
  with aMove, gPosition do
  begin
    b := board[sqFrom];
    board[sqFrom] := 0;
    board[sqTo] := b;
    enPassantSquare := NONE;
    case Abs(b) of
      PAWN:
        begin
          if Abs(sqFrom - sqTo) = 20 then
            enPassantSquare := (sqFrom + sqTo) div 2;
          case Abs(moveClass) of
            KNIGHT, BISHOP, ROOK, QUEEN:
              board[sqTo] := aColor * Abs(moveClass);
            ENPASSANT:
              board[sqTo + DIRPAWN[1] * aColor] := 0;
          end;
        end;
      KING:
        begin
          aKingSquare := sqTo;
          if kingCastle[aColor] then
          begin
            kingCastle[aColor] := FALSE;
            queenRookCastle[aColor] := FALSE;
            kingRookCastle[aColor] := FALSE;
          end;
          case moveClass of
            OO:
              begin
                board[Pred(sqTo)] := aColor * ROOK;
                board[sqFrom + 3] := 0;
              end;
            OOO:
              begin
                board[Succ(sqTo)] := aColor * ROOK;
                board[sqFrom - 4] := 0;
              end;
          end;
        end;
      ROOK:
        if sqFrom = SQQUEENROOKS[aColor] then
          queenRookCastle[aColor] := FALSE
        else if sqFrom = SQKINGROOKS[aColor] then
          kingRookCastle[aColor] := FALSE;
    end;
    if sqTo = SQQUEENROOKS[- 1 * aColor] then
      queenRookCastle[- 1 * aColor] := FALSE
    else if sqTo = SQKINGROOKS[- 1 * aColor] then
      kingRookCastle[- 1 * aColor] := FALSE;
  end;
end;

function FindMate(
  aColor: integer;
  aDepth: integer;
  aMaxDepth: integer;
  var aMove: TMoveRecord;
  aCheckOnly: boolean
): boolean;
label
  labelNext, labelMat;
var
  lKingSquare, lOtherKingSquare,
  i, j, k, k2, _: integer;
  lData1, lData2: TAuxiliaryData;
  lMove: TMoveRecord;
  lMoves1, lMoves2: TArrayOfMove;
  lMovesNumber1, lMovesNumber2: integer;
  lPositionRecord1, lPositionRecord2: TPositionRecord;
  lFound, lStalemate: boolean;
begin
  FillAuxiliaryData(lData1);

  lKingSquare := lData1[aColor].kingSquare;
  lOtherKingSquare := lData1[- 1 * aColor].kingSquare;

  lPositionRecord1 := gPosition;

  with lData1[aColor] do
    for k := 1 to piecesNumber do
    begin
      GenerateMoves(
        aColor,
        pieces[k],
        lMoves1,
        lMovesNumber1,
        lKingSquare,
        lOtherKingSquare,
        aDepth <> aMaxDepth,
        FALSE,
        lFound
      );
      for i := 1 to lMovesNumber1 do
      begin
        lMove := lMoves1[i];
        PerformMove(lMove, aColor, lKingSquare);
        if aDepth = aMaxDepth then
          if InCheck(- 1 * aColor, lOtherKingSquare, lKingSquare) then
          begin
            if InCheck(aColor, lKingSquare, lOtherKingSquare) then
              goto labelNext;

            if lMove.moveClass < 0 then
              FillAuxiliaryData(lData2)
            else
              lData2 := lData1;

            if AnyMoveSide(- 1 * aColor, lData2, lOtherKingSquare, lKingSquare) then
              goto labelNext;

            goto labelMat;
          end else
            goto labelNext;

        if aCheckOnly then
          if not InCheck(- 1 * aColor, lOtherKingSquare, lKingSquare) then
            goto labelNext;

        lStalemate := TRUE;

        if lMove.moveClass < 0 then
          FillAuxiliaryData(lData2)
        else
          lData2 := lData1;

        with lData2[- 1 * aColor] do
          for k2 := 1 to piecesNumber do
          begin
            GenerateMoves(
              - 1 * aColor,
              pieces[k2],
              lMoves2,
              lMovesNumber2,
              lOtherKingSquare,
              lKingSquare,
              TRUE,
              FALSE,
              lFound
            );

            if lMovesNumber2 <> 0 then
            begin
              lStalemate := FALSE;
              lPositionRecord2 := gPosition;
              for j := 1 to lMovesNumber2 do
              begin
                PerformMove(lMoves2[j], - 1 * aColor, _);
                if not FindMate(aColor, Succ(aDepth), aMaxDepth, aMove, aCheckOnly) then
                  goto labelNext;

                gPosition := lPositionRecord2;
              end;
            end;
          end;

        if aCheckOnly then
          goto labelMat;

        if lStalemate then
          if InCheck(- 1 * aColor, lOtherKingSquare, lKingSquare) then
            goto labelMat
          else
            goto labelNext;

        labelMat:

        if aDepth = 1 then
          aMove := lMove;
        result := TRUE;
        gPosition := lPositionRecord1;
        exit;

        labelNext:

        gPosition := lPositionRecord1;
        lKingSquare := kingSquare;

      end;
    end;
  result := FALSE;
end;

function SearchMate(
  aColor: integer;
  aDepth: integer;
  aMaxDepth: integer;
  var aMoveDepth: integer;
  var aMove: TMoveRecord;
  aCheckOnly: boolean
): boolean;
var
  i: integer;
begin
  result := FALSE;
  for i := aDepth to aMaxDepth do
  begin
    if FindMate(aColor, 1, i, aMove, aCheckOnly) then
    begin
      result := TRUE;
      aMoveDepth := i;
      exit;
    end;
  end;
end;

function SolveMate(
  const aFen: string;
  const aMovesNumber: integer;
  const aSearchAllMoves: boolean
): string;
var
  lMoveDepth: integer;
  lTurn: integer;
  lMove: TMoveRecord;
begin
  result := '';
  if InitializeGlobalPosition(aFen, lTurn) then
  begin
    gNodes := 0;
    
    if SearchMate(
      lTurn,
      1,
      aMovesNumber,
      lMoveDepth,
      lMove,
      not aSearchAllMoves
    ) then
      result := Concat(SquareToStr(lMove.sqFrom), SquareToStr(lMove.sqTo));
  end;
end;

const
  EMPTY_POSITION: TPositionRecord = (
    board: (
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 0, 0, 0, 0, 0, 0, 0, 0, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7
    );
    kingCastle: (FALSE, FALSE, FALSE);
    queenRookCastle: (FALSE, FALSE, FALSE);
    kingRookCastle: (FALSE, FALSE, FALSE);
    enPassantSquare: NONE
  );

begin
  gPosition := EMPTY_POSITION;
end.
