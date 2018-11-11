@echo off

if not exist units mkdir units
rem if not exist units\bass mkdir units\bass
if not exist units\bgrabitmap mkdir units\bgrabitmap
if not exist units\eschecs mkdir units\eschecs
if not exist units\fpgui mkdir units\fpgui

pushd libraries\fpgui\src
fpc -dGDI @..\..\..\config\fpgui.cfg corelib\gdi\fpgui_toolkit.pas

popd
fpc @config\bgrabitmap.cfg libraries\bgrabitmap\BGRABitmapPack4fpGUI.pas
rem fpc -MObjFPC -FUunits\bass\ libraries\bass\bass.pas
