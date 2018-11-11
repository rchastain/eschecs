@echo off

set ver=3.0.2
set path=c:\fpc\%ver%\bin\i386-win32

if exist %~dpn0.exe @(del %~dpn0.exe)

fpc.exe -Mdelphi -gl -FuC:\Sources\console %~dpn0.pas

if exist %~dpn0.exe @(%~dpn0.exe %1 > %~dpn0.log)
