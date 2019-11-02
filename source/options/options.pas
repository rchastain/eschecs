
program Options;

uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  SysUtils,
  Classes,
  
  fpg_main,
  fpg_stylemanager,
  
  FrmOptions,
  Style;

{$I icon.inc}
{$IFDEF WINDOWS}
{$R eschecs.res}
{$ENDIF}

procedure MainProc;
var
  frm: TConfigForm;
begin
  fpgApplication.Initialize;
  fpgImages.AddMaskedBMP('vfd.eschecs', @vfd_eschecs, SizeOf(vfd_eschecs), 0, 0);
  if fpgStyleManager.SetStyle('eschecs_style') then
    fpgStyle := fpgStyleManager.Style;
  frm := TConfigForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.
