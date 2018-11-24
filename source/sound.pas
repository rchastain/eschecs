
unit Sound;

{$mode objfpc}{$H+}

interface

const
 sndCapture = 0;
 sndCheck = 1;
 sndEndOfGame = 2;
 sndIllegal = 3;
 sndMove = 4;
 sndPromotion = 5;
    
function LoadSoundLib() : integer;
procedure Play(const aSound: integer);
procedure SetSoundVolume(vol : shortint);
procedure Freeuos;

implementation

uses
  SysUtils, Classes, uos_flat;
  
const
  FILENAME: array[0..5] of string = (
    'capture.mp3',
    'check.mp3',
    'endofgame.mp3',
    'illegal.mp3',
    'move.mp3',
    'promotion.mp3'
  );  
  
var
 ms :  array[0..5] of Tmemorystream; 
 isloaded : boolean = false;

procedure Play(const aSound: integer);
begin
if isloaded then uos_PlayNoFree(aSound);
end;

function LoadSoundLib() : integer;
var
 PA_FileName, MP_FileName, vsodir, vaudir: string;
  x : integer;
begin
 vaudir := ExtractFilePath(ParamStr(0)) + 'audio' + directoryseparator ;
 vsodir := vaudir +  'sound' + directoryseparator;
 
 {$IFDEF Windows}
    {$if defined(cpu64)}
    PA_FileName := vaudir + 'lib\Windows\64bit\LibPortaudio-64.dll';
    MP_FileName := vaudir + 'lib\Windows\64bit\LibMpg123-64.dll';
    {$else}
    PA_FileName := vaudir + 'lib\Windows\32bit\LibPortaudio-32.dll';
    MP_FileName := vaudir + 'lib\Windows\32bit\LibMpg123-32.dll';
    {$endif}
 {$ENDIF}
  {$if defined(cpu64) and defined(linux) }
    PA_FileName := vaudir + 'lib/Linux/64bit/LibPortaudio-64.so';
    MP_FileName := vaudir + 'lib/Linux/64bit/LibMpg123-64.so';
  {$ENDIF}
 {$if defined(cpu86) and defined(linux)}
    PA_FileName := vaudir + 'lib/Linux/32bit/LibPortaudio-32.so';
    MP_FileName := vaudir + 'lib/Linux/32bit/LibMpg123-32.so';
 {$ENDIF}
 {$if defined(cpuarm) and defined(linux)}
    PA_FileName := vaudir + 'lib/Linux/arm_raspberrypi/libportaudio-arm.so';
    MP_FileName := vaudir + 'lib/Linux/arm_raspberrypi/LibMpg123-arm.so';
 {$ENDIF}
 {$IFDEF freebsd}
    {$if defined(cpu64)}
    PA_FileName := vaudir + 'lib/FreeBSD/64bit/libportaudio-64.so';
    MP_FileName := vaudir + 'lib/FreeBSD/64bit/libmpg123-64.so';
    {$else}
    PA_FileName := vaudir + 'lib/FreeBSD/32bit/libportaudio-32.so';
    MP_FileName := vaudir + 'lib/FreeBSD/32bit/libmpg123-32.so';
    {$endif}
 {$ENDIF}
  
// Load the libraries, here only PortAudio and Mpg123
  result := uos_LoadLib(Pchar(PA_FileName), nil, Pchar(MP_FileName), nil, nil,  nil) ;

if result > -1 then begin 
// using memorystream
for x := 0 to 5 do
begin
ms[x] := TMemoryStream.Create;
ms[x].LoadFromFile(pchar(vsodir +  FILENAME[x])); 
ms[x].Position:= 0;
uos_CreatePlayer(x);
uos_AddFromMemoryStream(x,ms[x],1,-1,0,1024); 
 {$if defined(cpuarm)} // needs lower latency
uos_AddIntoDevOut(x, -1, 0.08, -1, -1, 0, 1024, -1);
 {$else}
uos_AddIntoDevOut(x, -1, 0.03, -1, -1, 0, 1024, -1);
 {$endif}
uos_OutputAddDSPVolume(x, 0, 1, 1); 
end;
isloaded := true;
end;
{$IFDEF OPT_DEBUG}
   WriteLn('Result of uos_LoadLib(): ' + inttostr(result));
{$ENDIF}
end;

procedure SetSoundVolume(vol : shortint);
var
x : integer;
begin
if isloaded then for x := 0 to 5 do uos_OutputSetDSPVolume(x, 0, vol/100, vol/100, True);
end;
   
procedure Freeuos;
begin
 uos_free();
end;
  
end.
