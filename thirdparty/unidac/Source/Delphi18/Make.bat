@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio XE4
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\RAD Studio\11.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 18 %1 %2
goto end
:all
call ..\Make.bat Delphi 18 WIN32
call ..\Make.bat Delphi 18 WIN64
call ..\Make.bat Delphi 18 OSX32
call ..\Make.bat Delphi 18 iOSSimulator
call ..\Make.bat Delphi 18 iOSDevice
:end
