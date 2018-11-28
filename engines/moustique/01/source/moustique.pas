
program Moustique;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  Player,
  Parser,
  Log;

{$INCLUDE VERSION.inc}

var
  vPosition: string;
  vMovesArray: array of string;
  
type
  TBestMoveThread = class(TThread)
    protected
      procedure Execute; override;
  end;

procedure TBestMoveThread.Execute;
var
  vMove: string;
begin
  vMove := Player.GetPlayerMove(vPosition, vMovesArray);
  if Length(vMovesArray) > 0 then SetLength(vMovesArray, 0);
  WriteLn(output, Format('bestmove %s', [vMove]));
  Flush(output);
end;

const
  CONVENTSTARTPOS = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

var
  vParser: TUCICommandParser;
  vCommand: string;
  vStop: boolean = false;
  vUciCommand: TUCICommand;
  vThread: TBestMoveThread;
  vIndex: integer;
  
begin
  TLog.Append(Format('Moustique %s %s %s FPC %s', [APPVERSION, {$I %DATE%}, {$I %TIME%}, {$I %FPCVERSION%}]));
  
  vParser := TUCICommandParser.Create;
  vPosition := CONVENTSTARTPOS;
  SetLength(vMovesArray, 0);
  
  repeat
    Sleep(10);
    ReadLn(input, vCommand);
    TLog.Append('>>> ' + vCommand);
    vUciCommand := vParser.ParseCommand(vCommand);

    case vUciCommand of
      cmdUCI:
        begin
          WriteLn(output, 'id name ' + APPNAME + ' ' + APPVERSION);
          WriteLn(output, 'id author ' + APPAUTHOR);
          WriteLn(output, 'uciok');
          Flush(output);
        end;

      cmdQuit:
        vStop := true;

      cmdNewGame:
        vPosition := CONVENTSTARTPOS;
      
      cmdPositionFen:
        vPosition := vParser.position;
        
      cmdPositionStartPos:
        begin
          vPosition := CONVENTSTARTPOS;
          if vParser.moves.Count > 0 then
          begin
            SetLength(vMovesArray, vParser.moves.Count);
            for vIndex := 0 to Pred(vParser.moves.Count) do
              vMovesArray[vIndex] := vParser.moves[vIndex];
          end;
        end;
        
      cmdGo:
        begin
          vThread := TBestMoveThread.Create(true);
          vThread.FreeOnTerminate := true;
          vThread.Priority := tpHigher;
          vThread.Start;
        end;
      
      cmdIsReady:
        begin
          WriteLn(output, 'readyok');
          Flush(output);
        end;
      
      cmdStop:
        begin
          //Player.DoReturn();
        end;
      
      cmdUnknown:
        begin
        end;
    end;
  until vStop;
  
  vParser.Free;
end.
