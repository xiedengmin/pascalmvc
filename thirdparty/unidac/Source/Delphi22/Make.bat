@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio XE8
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\Studio\16.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 22 %1 %2
goto end
:all
call ..\Make.bat Delphi 22 WIN32
call ..\Make.bat Delphi 22 WIN64
call ..\Make.bat Delphi 22 OSX32
call ..\Make.bat Delphi 22 iOSSimulator
call ..\Make.bat Delphi 22 iOSDevice32
call ..\Make.bat Delphi 22 iOSDevice64
call ..\Make.bat Delphi 22 Android
:end
