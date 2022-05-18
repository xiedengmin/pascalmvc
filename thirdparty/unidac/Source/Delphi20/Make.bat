@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio XE6
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\Studio\14.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 20 %1 %2
goto end
:all
call ..\Make.bat Delphi 20 WIN32
call ..\Make.bat Delphi 20 WIN64
call ..\Make.bat Delphi 20 OSX32
call ..\Make.bat Delphi 20 iOSSimulator
call ..\Make.bat Delphi 20 iOSDevice
call ..\Make.bat Delphi 20 Android
:end
