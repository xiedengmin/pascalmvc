@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio 10.4
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\Studio\21.0
set LinuxSDKDir=%USERPROFILE%\Documents\Embarcadero\Studio\SDKs\ubuntu16.04.sdk
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 28 %1 %2
goto end
:all
call ..\Make.bat Delphi 28 WIN32
call ..\Make.bat Delphi 28 WIN64
rem call ..\Make.bat Delphi 28 OSX64
rem call ..\Make.bat Delphi 28 Linux64
call ..\Make.bat Delphi 28 iOSSimulator
call ..\Make.bat Delphi 28 iOSDevice64
call ..\Make.bat Delphi 28 Android32
call ..\Make.bat Delphi 28 Android64
:end