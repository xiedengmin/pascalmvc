@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio 2009
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\CodeGear\RAD Studio\6.0
del /Q/S UniDAC\*.*

call ..\Make.bat Delphi 12 WIN32 %2
