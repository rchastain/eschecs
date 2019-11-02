
uses
  Uci;

const
  CMsg1 =
  'id name Pharaon 3.5.1'#13#10 +
  'id author Franck ZIBI'#13#10 +
  'option name Nullmove type check default TRUE'#13#10 +
  'option name Ponder type check default TRUE'#13#10 +
  'option name Clear Hash type button'#13#10 +
  'option name Hash type spin min 1 max 1024 default 64'#13#10 +
  'option name NalimovPath type string default ;./TB;C:\CHESS\TB;d:\Pharaon\tb;'#13#10 +
  'option name NalimovCache type spin min 1 max 32 default 4'#13#10 +
  'option name Number of threads type spin min 1 max 4 default 1'#13#10 +
  'option name UCI_Chess960 type check default FALSE'#13#10 +
  'copyprotection checking'#13#10 +
  'copyprotection ok'#13#10 +
  'uciok';
  CMsg2 = 'bestmove a7a8q';
  CMsg3 = 'bestmove a7a8 ';

var
  LName, LAuthor, LMove, LPromo: string;
  
begin
  WriteLn(MsgUci() = 'uci');
  
  WriteLn(IsMsgUciOk(CMsg1, LName, LAuthor));
  WriteLn(LName = 'Pharaon 3.5.1');
  
  WriteLn(IsMsgBestMove(CMsg2, LMove, LPromo));
  WriteLn(LPromo = 'q');
  
  WriteLn(IsMsgBestMove(CMsg3, LMove, LPromo));
  WriteLn(LPromo = '');
end.
