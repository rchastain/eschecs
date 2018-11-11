@echo off

if "%1" == "" (
  set root=%~dp0
) else (
  set root=%1
)

for /r %root% /d %%d in (backup cache __history __recovery) do if exist "%%d" (
  echo   %%d
  rd /s /q "%%d"
)

for /r %root% %%f in (*.compiled *.dbg *.dcu *.exe *.identcache *.local *.log *.lps *.o *.or *.ppu *.~*) do (
  del "%%f" > nul 2>&1
  if not exist "%%f" echo   %%f
)
