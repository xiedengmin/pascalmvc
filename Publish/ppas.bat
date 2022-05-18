@echo off
SET THEFILE=D:\my\LazarusMVC\Publish\MVCDemo.exe
echo Linking %THEFILE%
D:\lazarus\fpc\3.2.2\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  --gc-sections    --entry=_mainCRTStartup    -o D:\my\LazarusMVC\Publish\MVCDemo.exe D:\my\LazarusMVC\Publish\link2472.res
if errorlevel 1 goto linkend
D:\lazarus\fpc\3.2.2\bin\i386-win32\postw32.exe --subsystem console --input D:\my\LazarusMVC\Publish\MVCDemo.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
