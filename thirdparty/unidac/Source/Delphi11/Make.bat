@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio 2007
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\CodeGear\RAD Studio\5.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 11 %1 %2
goto end
:all
call ..\Make.bat Delphi 11 WIN32
rem call ..\Make.bat Delphi 11 CLR
:end
