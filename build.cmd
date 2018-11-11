@echo off

echo %~n0^(%1^)

cd source

set lang=%1
if [%lang%] == [] (
  set lang=french
)

copy languages\%lang%.inc language.inc

fpc eschecs.pas @..\config\eschecs.cfg

cd ..
