@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio 10
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\Studio\17.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 23 %1 %2
goto end
:all
call ..\Make.bat Delphi 23 WIN32
call ..\Make.bat Delphi 23 WIN64
call ..\Make.bat Delphi 23 OSX32
call ..\Make.bat Delphi 23 iOSSimulator
call ..\Make.bat Delphi 23 iOSDevice32
call ..\Make.bat Delphi 23 iOSDevice64
call ..\Make.bat Delphi 23 Android
:end
