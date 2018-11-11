
{$I DIRECTIVES}

uses
  SysUtils, Classes, IOUtils, FLRE;

const
  PATTERN = '([A-E][0-9]{2}) ([A-Z][ \w():''\-,/\.]+)\r\n *1\.([\sa-h1-8]+[1-8])';

var
  expr: TFLRE;
  ss: TFLREMultiStrings;
  i: integer;
  aux1: ansistring;
  //aux2: ansistring;
  
begin  
  expr := TFLRE.Create(PATTERN, []);

  if expr.ExtractAll(RawByteString(TFile.ReadAllText('ECO.TXT')), ss) then for i := 0 to Length(ss) - 1 do
  begin
    aux1 := StringReplace(ss[i, 3], #32, '', [rfReplaceAll]);
    aux1 := StringReplace(aux1, #13, '', [rfReplaceAll]);
    aux1 := StringReplace(aux1, #10, '', [rfReplaceAll]);
    (*
    aux2 := StringReplace(ss[i, 2], '''', '''''', [rfReplaceAll]);
    WriteLn(Format('(code: ''%s''; name: ''%s''; moves: ''%s''),', [ss[i, 1], aux2, aux1]));
    *)
    WriteLn(Format('"%s" : "%s-%s",', [aux1, ss[i, 1], ss[i, 2]]));
  end;

  SetLength(ss, 0);
  expr.Free;
end.
