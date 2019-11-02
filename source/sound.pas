
unit Sound;

interface

const
  sndCapture = 0;
  sndCheck = 1;
  sndEndOfGame = 2;
  sndIllegal = 3;
  sndMove = 4;
  sndPromotion = 5;
    
function LoadSoundLib: integer;
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

function LoadSoundLib: integer;
var
  PA_FileName, MP_FileName, vsodir, vaudir: string;
  x: integer;
begin
  vaudir := ExtractFilePath(ParamStr(0)) + 'audio' + DirectorySeparator ;
  vsodir := vaudir + 'sound' + DirectorySeparator;
{$ifdef windows}
{$if defined(cpu64)}
  PA_FileName := vaudir + 'lib\Windows\64bit\LibPortaudio-64.dll';
  MP_FileName := vaudir + 'lib\Windows\64bit\LibMpg123-64.dll';
{$else}
  PA_FileName := vaudir + 'lib\Windows\32bit\LibPortaudio-32.dll';
  MP_FileName := vaudir + 'lib\Windows\32bit\LibMpg123-32.dll';
{$endif}
{$endif}
{$if defined(cpu64) and defined(linux) }
  PA_FileName := vaudir + 'lib/Linux/64bit/LibPortaudio-64.so';
  MP_FileName := vaudir + 'lib/Linux/64bit/LibMpg123-64.so';
{$endif}
{$if defined(cpu86) and defined(linux)}
  PA_FileName := vaudir + 'lib/Linux/32bit/LibPortaudio-32.so';
  MP_FileName := vaudir + 'lib/Linux/32bit/LibMpg123-32.so';
{$endif}
{$if defined(cpuarm) and defined(linux)}
  PA_FileName := vaudir + 'lib/Linux/arm_raspberrypi/libportaudio-arm.so';
  MP_FileName := vaudir + 'lib/Linux/arm_raspberrypi/LibMpg123-arm.so';
{$endif}
{$ifdef freebsd}
{$if defined(cpu64)}
  PA_FileName := vaudir + 'lib/FreeBSD/64bit/libportaudio-64.so';
  MP_FileName := vaudir + 'lib/FreeBSD/64bit/libmpg123-64.so';
{$else}
  PA_FileName := vaudir + 'lib/FreeBSD/32bit/libportaudio-32.so';
  MP_FileName := vaudir + 'lib/FreeBSD/32bit/libmpg123-32.so';
{$endif}
{$endif}
  result := uos_LoadLib(PChar(PA_FileName), nil, PChar(MP_FileName), nil, nil,  nil) ;
  if result > -1 then
  begin 
    for x := 0 to 5 do
    begin
      LStreams[x] := TMemoryStream.Create;
      LStreams[x].LoadFromFile(PChar(vsodir + CFileName[x])); 
      LStreams[x].Position:= 0;
      uos_CreatePlayer(x);
      uos_AddFromMemoryStream(x, LStreams[x], 1, -1, 0, 1024); 
{$if defined(cpuarm)}
      uos_AddIntoDevOut(x, -1, 0.08, -1, -1, 0, 1024, -1);
{$else}
      uos_AddIntoDevOut(x, -1, 0.03, -1, -1, 0, 1024, -1);
{$endif}
      uos_OutputAddDSPVolume(x, 0, 1, 1); 
    end;
    LLoaded := TRUE;
  end;
end;

procedure SetSoundVolume(AVol: shortint);
var
  x: integer;
begin
  if LLoaded then
    for x := 0 to 5 do
      uos_OutputSetDSPVolume(x, 0, AVol / 100, AVol / 100, TRUE);
end;
   
procedure FreeUos;
begin
  uos_free;
end;
  
end.
