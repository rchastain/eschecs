@echo off

rem c:\outils\pasdoc\bin\pasdoc myunit.pas
rem c:\outils\pasdoc\bin\pasdoc --language=fr myunit.pas

set pasdoc=c:\outils\pasdoc\bin\pasdoc.exe

set output=.\documentation\
if exist %output% rd /s /q %output%
if not exist %output% md %output%

rem %pasdoc% --output=%output% --language=fr.utf8 --staronly demo.pas
%pasdoc% --output=%output% --language=fr.utf8 --staronly --source=builddoc.cfg

pause
