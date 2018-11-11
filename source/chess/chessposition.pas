
{**
@abstract(Unité contenant la classe TChessPosition.)
}
unit chessposition;

interface

uses
  sysutils,
  classes,
  chesstypes,
  fen,
  chessutils;

type
  TCastling = (e1g1, e1c1, e8g8, e8c8);
  
  TChessPosition = class
    private
      fData: TChessPositionData;
      fCheck: boolean;
      fCastlingCheck: array[TCastling] of boolean;
      fList: TStringList;
      fKingCheckedX: integer;
      fKingCheckedY: integer;
    public
      constructor Create; overload;
      constructor Create(const aFENRecord: string); overload;
      destructor Destroy; override;
      function FENRecord: string;
      procedure GenerateMoves1(const aColor: TChessPieceColor);
      procedure GenerateMoves2(const aColor: TChessPieceColor);
      procedure GenerateCastlingMove(const aCastling: TCastling);
      function IsCheck(const aColor: TChessPieceColor): boolean;
      function IsCastlingCheck(const aCastling: TCastling): boolean;
      procedure SetVariables;
      procedure PlayMove(const aMove: string; const aPromotion: TChessPieceKind = cpkQueen);
      procedure CastlingRemove(const aCastling: TCastling);
      procedure SetActiveColor(const aColor: TChessPieceColor);
      property data: TChessPositionData read fData;
      property list: TStringList read fList;
      property activeColor: TChessPieceColor read fData.activeColor write SetActiveColor;
      property check: boolean read fCheck;
      property kingCheckedX: integer read fKingCheckedX;
      property kingCheckedY: integer read fKingCheckedY;
  end;

implementation

constructor TChessPosition.Create;
begin
  inherited Create;
  if fList = nil then fList := TStringList.Create else fList.Clear;
  fList.Sorted := true;
  fKingCheckedX := 0;
  fKingCheckedY := 0;
end;

constructor TChessPosition.Create(const aFENRecord: string);
begin
  Create;
  fData := EncodeChessPositionData(aFENRecord);
end;

destructor TChessPosition.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

function TChessPosition.FENRecord: string;
begin
  result := DecodeChessPositionData(fData);
end;

procedure TChessPosition.GenerateMoves1(const aColor: TChessPieceColor);
var
  x1, y1, x2, y2: integer;
  vectorIndex, index1, index2: integer;
begin
  fList.Clear;
  
  for x1 := 1 to 8 do
    for y1 := 1 to 8 do
      //if Assigned(fData.board[x1, y1])
      //if (fData.board[x1, y1].kind <> cpkNil)
      //and (fData.board[x1, y1].color = aColor) then
      if (fData.board[x1, y1].color = aColor) then
        case fData.board[x1, y1].kind of
          cpkPawn:
            begin
              if aColor = cpcWhite then
              begin
                index1 := 1;
                index2 := 2;
              end else
              begin
                index1 := 3;
                index2 := 4;
              end;
              for vectorIndex := index1 to index2 do
                if ComputeTargetSquare(x1, y1, vectorIndex, x2, y2)
                //and Assigned(fData.board[x2, y2])
                and (fData.board[x2, y2].color = OtherColor(aColor))
                then
                  fList.Add(MoveToStr(x1, y1, x2, y2));
            end;
          
          cpkKnight:
            for vectorIndex := 9 to 16 do
              if ComputeTargetSquare(x1, y1, vectorIndex, x2, y2)
              //and ((fData.board[x2, y2].kind = cpkNil) or (fData.board[x2, y2].color = OtherColor(aColor))) then
              and (fData.board[x2, y2].color <> aColor) then
                fList.Add(MoveToStr(x1, y1, x2, y2));
          
          cpkBishop, cpkRook, cpkQueen:
            begin
              case fData.board[x1, y1].kind of
                cpkBishop:
                  begin
                    index1 := 1;
                    index2 := 4;
                  end;
                cpkRook:
                  begin
                    index1 := 5;
                    index2 := 8;
                  end;
                cpkQueen:
                  begin
                    index1 := 1;
                    index2 := 8;
                  end;
              end;
              
              for vectorIndex := index1 to index2 do
              begin
                x2 := x1;
                y2 := y1;
                repeat
                  if ComputeTargetSquare(x2, y2, vectorIndex, x2, y2)
                  //and ((fData.board[x2, y2] = nil) or (fData.board[x2, y2].color = OtherColor(aColor))) then
                  and (fData.board[x2, y2].color <> aColor) then
                    fList.Add(MoveToStr(x1, y1, x2, y2))
                  else
                    Break;
                until fData.board[x2, y2].kind <> cpkNil;//Assigned(fData.board[x2, y2]);
              end;
            end;
          
          cpkKing:
            for vectorIndex := 1 to 8 do
              if ComputeTargetSquare(x1, y1, vectorIndex, x2, y2)
              //and ((fData.board[x2, y2] = nil) or (fData.board[x2, y2].color = OtherColor(aColor))) then
              and (fData.board[x2, y2].color <> aColor) then
                fList.Add(MoveToStr(x1, y1, x2, y2));
        end;
end;

procedure TChessPosition.GenerateMoves2(const aColor: TChessPieceColor);
var
  x1, y1, x2, y2: integer;
  vectorIndex, index1, index2: integer;
begin
  for x1 := 1 to 8 do
    for y1 := 1 to 8 do
      //if Assigned(fData.board[x1, y1])
      //and (fData.board[x1, y1].color = aColor) then
      if fData.board[x1, y1].color = aColor then
        case fData.board[x1, y1].kind of
          cpkPawn:
            begin
              if aColor = cpcWhite then
              begin
                index1 := 1;
                index2 := 2;
              end else
              begin
                index1 := 3;
                index2 := 4;
              end;
              for vectorIndex := index1 to index2 do
                if ComputeTargetSquare(x1, y1, vectorIndex, x2, y2)
                and (fData.board[x2, y2].kind = cpkNil)
                and (SquareToStr(x2, y2) = fData.enPassant)
                then
                  fList.Add(MoveToStr(x1, y1, x2, y2));
              
              if aColor = cpcWhite then
                vectorIndex := 7
              else
                vectorIndex := 8;
              if ComputeTargetSquare(x1, y1, vectorIndex, x2, y2)
              and (fData.board[x2, y2].kind = cpkNil) then
              begin
                fList.Add(MoveToStr(x1, y1, x2, y2));
                
                if (((aColor = cpcWhite) and (y2 = 3)) or ((aColor = cpcBlack) and (y2 = 6)))
                and ComputeTargetSquare(x2, y2, vectorIndex, x2, y2)
                and (fData.board[x2, y2].kind = cpkNil) then
                  fList.Add(MoveToStr(x1, y1, x2, y2));
              end;
            end;
          
          cpkKing:
            if aColor = cpcWhite then
            begin
              if Pos('K', fData.castling) > 0 then GenerateCastlingMove(e1g1);
              if Pos('Q', fData.castling) > 0 then GenerateCastlingMove(e1c1);
            end else
            begin
              if Pos('k', fData.castling) > 0 then GenerateCastlingMove(e8g8);
              if Pos('q', fData.castling) > 0 then GenerateCastlingMove(e8c8);
            end;
        end;
end;

procedure TChessPosition.GenerateCastlingMove(const aCastling: TCastling);
var
  x, y,
  x1, x2: integer;
  condition: boolean;
begin
  condition := true;
  
  case aCastling of
    e1g1: begin y := 1; x1 := 5; x2 := 8; end;
    e1c1: begin y := 1; x1 := 1; x2 := 5; end;
    e8g8: begin y := 8; x1 := 5; x2 := 8; end;
    e8c8: begin y := 8; x1 := 1; x2 := 5; end;
  end;
  for x := x1 + 1 to x2 - 1 do
    if fData.board[x, y].kind <> cpkNil then
      condition := false;
  
  if condition
  and not fCastlingCheck[aCastling] then
    fList.Add(MoveToStr(5, y, 3 + 4 * Ord(aCastling in [e1g1, e8g8]), y));
end;

function TChessPosition.IsCheck(const aColor: TChessPieceColor): boolean;
var
  i: integer;
  x2, y2: integer;
begin
  result := false;
  for i := 0 to fList.Count - 1 do
  begin
    StrToSquare(Copy(fList[i], 3, 2), x2, y2);
    
    if (fData.board[x2, y2].kind = cpkKing)
    and (fData.board[x2, y2].color = aColor) then
    begin
      result := true;
      fKingCheckedX := x2;
      fKingCheckedY := y2;
    end;
  end;
end;

function TChessPosition.IsCastlingCheck(const aCastling: TCastling): boolean;
var
  i: integer;
  x1, y1, x2, y2: integer;
  row, column1, column2: integer;
  color: TChessPieceColor;
begin
  case aCastling of
    e1g1: begin row := 1; column1 := 5; column2 := 8; color := cpcBlack; end;
    e1c1: begin row := 1; column1 := 1; column2 := 5; color := cpcBlack; end;
    e8g8: begin row := 8; column1 := 5; column2 := 8; color := cpcWhite; end;
    e8c8: begin row := 8; column1 := 1; column2 := 5; color := cpcWhite; end;
  end;
  result := false;
  for i := 0 to fList.Count - 1 do
  begin
    StrToMove(fList[i], x1, y1, x2, y2);
    
    if (y2 = row)
    and (x2 in [column1..column2])
    and (fData.board[x1, y1].color = color) then
      result := true;
  end;
end;

procedure TChessPosition.SetVariables;
var
  c: TCastling;
begin
  GenerateMoves1(OtherColor(fData.activeColor));
  
  fCheck := IsCheck(fData.activeColor);
  if not fCheck then
  begin
    fKingCheckedX := 0;
    fKingCheckedY := 0;
  end;
  
  for c in TCastling do
    fCastlingCheck[c] := fCheck or IsCastlingCheck(c);
end;

procedure TChessPosition.PlayMove(const aMove: string; const aPromotion: TChessPieceKind = cpkQueen);
  procedure MovePiece(x1, y1, x2, y2: integer);
  begin
    fData.board[x2, y2] := fData.board[x1, y1];
    fData.board[x1, y1].kind := cpkNil;
    fData.board[x1, y1].color := cpcNil;
  end;
var
  x1, y1, x2, y2: integer;
begin
  StrToMove(aMove, x1, y1, x2, y2);
  
  if fData.board[x1, y1].kind = cpkNil then
    exit;
  
  if fData.board[x1, y1].kind = cpkKing then
    case 10 * x1 + y1 of
      51:
        begin
          CastlingRemove(e1g1);
          CastlingRemove(e1c1);
        end;
      58:
        begin
          CastlingRemove(e8g8);
          CastlingRemove(e8c8);
        end;
    end;
  
  if fData.board[x1, y1].kind = cpkRook then
    case 10 * x1 + y1 of
      81: CastlingRemove(e1g1);
      11: CastlingRemove(e1c1);
      88: CastlingRemove(e8g8);
      18: CastlingRemove(e8c8);
    end;
  
  if (fData.board[x1, y1].kind = cpkPawn) and (Abs(y2 - y1) = 2) then
    fData.enPassant := SquareToStr(x1, y1 - 2 * Ord(fData.activeColor) + 1)
  else
    fData.enPassant := '-';
  
  if (fData.board[x1, y1].kind = cpkKing) and (Abs(x2 - x1) = 2) then
    case 10 * x2 + y2 of
      71: MovePiece(8, 1, 6, 1);
      31: MovePiece(1, 1, 4, 1);
      78: MovePiece(8, 8, 6, 8);
      38: MovePiece(1, 8, 4, 8);
    end;
  
  if (fData.board[x1, y1].kind = cpkPawn) and (x2 <> x1) and (fData.board[x2, y2].kind = cpkNil) then
  begin
    fData.board[x2, y1].color := cpcNil;
    fData.board[x2, y1].kind := cpkNil;
  end;
  
  if (fData.board[x1, y1].kind = cpkPawn) or (fData.board[x2, y2].kind <> cpkNil) then
    fData.halfMoves := 0
  else
    Inc(fData.halfMoves);
  
  if fData.activeColor = cpcBlack then
    Inc(fData.fullMove);
  
  if (fData.board[x1, y1].kind = cpkPawn) and ((y2 = 1) or (y2 = 8)) then
    fData.board[x1, y1].kind := aPromotion;
  
  MovePiece(x1, y1, x2, y2);
  
  fData.activeColor := OtherColor(fData.activeColor);
end;

procedure TChessPosition.CastlingRemove(const aCastling: TCastling);
var
  i: integer;
  c: char;
begin
  case aCastling of
    e1g1: c := 'K';
    e1c1: c := 'Q';
    e8g8: c := 'k';
    e8c8: c := 'q';
  end;
  i := Pos(c, fData.castling);
  if i > 0 then
    if Length(fData.castling) = 1 then
      fData.castling := '-'
    else
      Delete(fData.castling, i, 1);
end;

procedure TChessPosition.SetActiveColor(const aColor: TChessPieceColor);
begin
  fData.activeColor := aColor;
end;

end.
