@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio XE3
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\RAD Studio\10.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 17 %1 %2
goto end
:all
call ..\Make.bat Delphi 17 WIN32
call ..\Make.bat Delphi 17 WIN64
call ..\Make.bat Delphi 17 OSX32
:end
