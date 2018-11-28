
unit Parser;

interface

uses
  Classes;

type
  TUCICommand =
  (
    cmdUCI,
    cmdQuit,
    cmdNewGame,
    cmdPositionFen,
    cmdPositionStartPos,
    cmdGo,
    cmdIsReady,
    cmdStop,
    cmdUnknown
  );

  TUCICommandParser = class
    //private
      moves: TStringList;
      position: string;
    //protected
      constructor Create;
      destructor Destroy; override;
    //public
      function ParseCommand(const aCommand: string): TUCICommand;
  end;

implementation

uses
  Expressions;

constructor TUCICommandParser.Create;
begin
  moves := Expressions.list;
end;

destructor TUCICommandParser.Destroy;
begin
  inherited Destroy;
end;

function TUCICommandParser.ParseCommand(const aCommand: string): TUCICommand;
begin
  if aCommand = 'uci' then
    result := cmdUCI
  else
    if aCommand = 'quit' then
      result := cmdQuit
    else
      if aCommand = 'ucinewgame' then
        result := cmdNewGame
      else
        if (Pos('position fen', aCommand) = 1)
        and ExtractFEN(aCommand, position) then
          result := cmdPositionFen
        else
          if (Pos('position startpos', aCommand) = 1) then
          begin
            result := cmdPositionStartPos;
            ExtractMoves(aCommand);
          end else
            if Pos('go', aCommand) = 1 then
              result := cmdGo
            else
              if aCommand = 'isready' then
                result := cmdIsReady
              else
                if aCommand = 'stop' then
                  result := cmdStop
                else
                  result := cmdUnknown;
end;

end.
