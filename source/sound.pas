
unit Sound;

interface

type
  TSound = (
    sndCapture,
    sndCheck,
    sndEndOfGame,
    sndIllegal,
    sndMove,
    sndPromotion
  );

procedure Play(const aSound: TSound);

implementation

uses
  SysUtils, Bass;
  
var
  vSamples: array[TSound] of HSAMPLE;

procedure Play(const aSound: TSound);
begin
  BASS_ChannelPlay(BASS_SampleGetChannel(vSamples[aSound], FALSE), FALSE);
end;

const
  FILENAME: array[TSound] of string = (
    'capture.mp3',
    'sfx_alarm_loop2.wav',
    'genericnotify.mp3',
    'illegal.wav',
    'move.mp3',
    'sfx_coin_cluster1.wav'
  );
  
var
  vSound: TSound;
  vFileName: string;
  
initialization
  BASS_Init(-1, 44100, 0, 0, nil);
  
  for vSound in TSound do
  begin
    vFileName := 'sounds\' + FILENAME[vSound];
    if FileExists(vFileName)
    then
      vSamples[vSound] := BASS_SampleLoad(
        FALSE,
        pchar(vFileName),
        0,
        0,
        1,
        0
      )
    else
    begin
{$IFDEF DEBUG}
      WriteLn(Format('Fichier introuvable ! %s', [vFileName]));
{$ENDIF}
    end;
  end;
      
finalization
  BASS_Free;
  
end.
