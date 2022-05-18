@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio 10.1
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\Studio\18.0
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 24 %1 %2
goto end
:all
call ..\Make.bat Delphi 24 WIN32
call ..\Make.bat Delphi 24 WIN64
call ..\Make.bat Delphi 24 OSX32
call ..\Make.bat Delphi 24 iOSSimulator
call ..\Make.bat Delphi 24 iOSDevice32
call ..\Make.bat Delphi 24 iOSDevice64
call ..\Make.bat Delphi 24 Android
:end
