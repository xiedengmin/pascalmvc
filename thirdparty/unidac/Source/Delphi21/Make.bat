@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio XE7
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\Studio\15.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 21 %1 %2
goto end
:all
call ..\Make.bat Delphi 21 WIN32
call ..\Make.bat Delphi 21 WIN64
call ..\Make.bat Delphi 21 OSX32
call ..\Make.bat Delphi 21 iOSSimulator
call ..\Make.bat Delphi 21 iOSDevice
call ..\Make.bat Delphi 21 Android
:end
