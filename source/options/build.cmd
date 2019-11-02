@echo off
set fpc=C:\lazarus\fpc\3.0.4\bin\x86_64-win64\fpc.exe
%fpc% -dGDI @extrafpc.cfg options.pas
