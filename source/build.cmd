@echo off
set path=C:\lazarus\fpc\3.0.4\bin\x86_64-win64
rem set path=C:\FPC\3.0.4\bin\i386-win32
fpc -dDEBUG -dGDI @extrafpc.cfg eschecs.pas
rem fpc -dGDI @extrafpc.cfg eschecs.pas
