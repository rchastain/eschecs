
uses
  UCI;

const
  MSG1 =
  'id name Pharaon 3.5.1'#13#10 +
  'id author Franck ZIBI'#13#10 +
  'option name Nullmove type check default true'#13#10 +
  'option name Ponder type check default true'#13#10 +
  'option name Clear Hash type button'#13#10 +
  'option name Hash type spin min 1 max 1024 default 64'#13#10 +
  'option name NalimovPath type string default ;./TB;C:\CHESS\TB;d:\Pharaon\tb;'#13#10 +
  'option name NalimovCache type spin min 1 max 32 default 4'#13#10 +
  'option name Number of threads type spin min 1 max 4 default 1'#13#10 +
  'option name UCI_Chess960 type check default false'#13#10 +
  'copyprotection checking'#13#10 +
  'copyprotection ok'#13#10 +
  'uciok';
  MSG2 = 'bestmove a7a8q';
  MSG3 = 'bestmove a7a8 ';

var
  vEngineName, vAuthor, vBestMove, vPromotion: string;
  
begin
  WriteLn(MsgUCI() = 'uci');
  
  WriteLn(IsMsgUciOk(MSG1, vEngineName, vAuthor));
  WriteLn(vEngineName = 'Pharaon 3.5.1');
  
  WriteLn(IsMsgBestMove(MSG2, vBestMove, vPromotion));
  WriteLn(vPromotion = 'q');
  
  WriteLn(IsMsgBestMove(MSG3, vBestMove, vPromotion));
  WriteLn(vPromotion = '');
end.
