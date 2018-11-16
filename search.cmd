@echo off

call cmdmax.cmd

title %~nx0

set /p what="search what? "
set where=%~dp0*.*
findstr /s /i /n /p %what% %where%

rem cmdsave %~n0.log
rem cmdsave %~n0-%what%.log

pause
