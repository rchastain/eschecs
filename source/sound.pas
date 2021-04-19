
unit Sound;

interface

const
  sndCapture = 0;
  sndCheck = 1;
  sndEndOfGame = 2;
  sndIllegal = 3;
  sndMove = 4;
  sndPromotion = 5;
    
function LoadSoundLib(const AUseSystemLib: boolean = FALSE): integer;
procedure Play(const ASound: integer);
procedure SetSoundVolume(AVol: shortint);
procedure FreeUos;

implementation

uses
  SysUtils, Classes,
  uos_flat;
  
const
  CFileName: array[0..5] of string = (
    'capture.mp3',
    'check.mp3',
    'endofgame.mp3',
    'illegal.mp3',
    'move.mp3',
    'promotion.mp3'
  );  
  
var
  LStreams:  array[0..5] of TMemoryStream; 
  LLoaded: boolean = FALSE;

procedure Play(const ASound: integer);
begin
  if LLoaded then
    uos_PlayNoFree(ASound);
end;

function LoadSoundLib(const AUseSystemLib: boolean): integer;
var
  LPortAudio, LMpg123, LAudioPath, LSndFilesPath: string;
  x: integer;
begin
  //WriteLn('LoadSoundLib(', AUseSystemLib, ')');
  LAudioPath := ExtractFilePath(ParamStr(0)) + 'audio' + DirectorySeparator;
  LSndFilesPath := LAudioPath + 'sound' + DirectorySeparator;
{$IFDEF windows}
{$IF DEFINED(cpu64)}
  LPortAudio := LAudioPath + 'lib\Windows\64bit\LibPortaudio-64.dll';
  LMpg123 := LAudioPath + 'lib\Windows\64bit\LibMpg123-64.dll';
{$ELSE}
  LPortAudio := LAudioPath + 'lib\Windows\32bit\LibPortaudio-32.dll';
  LMpg123 := LAudioPath + 'lib\Windows\32bit\LibMpg123-32.dll';
{$ENDIF}
{$ENDIF}
{$IF DEFINED(cpu64) and defined(linux) }
  LPortAudio := LAudioPath + 'lib/Linux/64bit/LibPortaudio-64.so';
  LMpg123 := LAudioPath + 'lib/Linux/64bit/LibMpg123-64.so';
{$ENDIF}
{$IF DEFINED(cpu86) and defined(linux)}
  LPortAudio := LAudioPath + 'lib/Linux/32bit/LibPortaudio-32.so';
  LMpg123 := LAudioPath + 'lib/Linux/32bit/LibMpg123-32.so';
{$ENDIF}
{$IF DEFINED(cpuarm) and defined(linux)}
  LPortAudio := LAudioPath + 'lib/Linux/arm_raspberrypi/libportaudio-arm.so';
  LMpg123 := LAudioPath + 'lib/Linux/arm_raspberrypi/LibMpg123-arm.so';
{$ENDIF}
{$IFDEF freebsd}
{$IF DEFINED(cpu64)}
  LPortAudio := LAudioPath + 'lib/FreeBSD/64bit/libportaudio-64.so';
  LMpg123 := LAudioPath + 'lib/FreeBSD/64bit/libmpg123-64.so';
{$ELSE}
  LPortAudio := LAudioPath + 'lib/FreeBSD/32bit/libportaudio-32.so';
  LMpg123 := LAudioPath + 'lib/FreeBSD/32bit/libmpg123-32.so';
{$ENDIF}
{$ENDIF}
  
  if AUseSystemLib then
    result := uos_LoadLib('system', nil, 'system', nil, nil, nil)
  else
    result := uos_LoadLib(PChar(LPortAudio), nil, PChar(LMpg123), nil, nil,  nil);
  
  if result > -1 then
  begin 
    for x := 0 to 5 do
    begin
      LStreams[x] := TMemoryStream.Create;
      LStreams[x].LoadFromFile(PChar(LSndFilesPath + CFileName[x])); 
      LStreams[x].Position:= 0;
      uos_CreatePlayer(x);
      uos_AddFromMemoryStream(x, LStreams[x], 1, -1, 0, 1024); 
{$IF DEFINED(cpuarm)}
      uos_AddIntoDevOut(x, -1, 0.08, -1, -1, 0, 1024, -1);
{$ELSE}
      uos_AddIntoDevOut(x, -1, 0.03, -1, -1, 0, 1024, -1);
{$ENDIF}
      uos_OutputAddDSPVolume(x, 0, 1, 1); 
    end;
    LLoaded := TRUE;
  end;
end;

procedure SetSoundVolume(AVol: shortint);
var
  x: integer;
begin
  //WriteLn('SetSoundVolume(', AVol, ')');
  if LLoaded then
    for x := 0 to 5 do
      uos_OutputSetDSPVolume(x, 0, AVol / 100, AVol / 100, TRUE);
end;
   
procedure FreeUos;
begin
  uos_free;
end;
  
end.
