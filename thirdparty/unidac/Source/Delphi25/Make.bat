@echo off
rem **********************************************************************
rem *
rem * Unidac for RAD Studio 10.2
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\Studio\19.0
set LinuxSDKDir=%USERPROFILE%\Documents\Embarcadero\Studio\SDKs\ubuntu16.04.sdk
del /Q/S UniDAC\*.*

if "%1"=="" goto all
call ..\Make.bat Delphi 25 %1 %2
goto end
:all
call ..\Make.bat Delphi 25 WIN32
call ..\Make.bat Delphi 25 WIN64
call ..\Make.bat Delphi 25 OSX32
rem call ..\Make.bat Delphi 25 Linux64
call ..\Make.bat Delphi 25 iOSSimulator
call ..\Make.bat Delphi 25 iOSDevice32
call ..\Make.bat Delphi 25 iOSDevice64
call ..\Make.bat Delphi 25 Android
:end
