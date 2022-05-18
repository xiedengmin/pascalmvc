echo off
rem Path to DCC32.exe
SET DCCPATH=C:\Program Files\Embarcadero\RAD Studio\18.0\bin\

rem Path to project Dir
SET PROJECTPATH=C:\Program Files\Devart\UniDAC for RAD Studio 10.1\Demos\ThirdParty\FastReport\FR4\Delphi24\

rem FastReport 4 LibD24 Path
SET FRLIBDLLPATH=C:\Program Files\FastReports\FastReport 4\LibD24


SET IDEVer=24

SET DACname=UniDAC

SET AddOptions=-N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac%IDEVer%0;fs%IDEVer%;fsDB%IDEVer%" -NSData;System;Vcl;Winapi


echo on
rem -----------------------BEGIN------------------------------------------
echo off
cd "%PROJECTPATH%"

rem FastReport
"%DCCPATH%DCC32.EXE" -LE. -B -JL frxDAC%IDEVer%.dpk %AddOptions%
"%DCCPATH%DCC32.EXE" -LE. -B -JL frx%DACname%%IDEVer%.dpk %AddOptions%
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfrx%DACname%%IDEVer%.dpk %AddOptions%
rem FastScript
"%DCCPATH%DCC32.EXE" -LE. -B -JL fsDAC%IDEVer%.dpk %AddOptions%
"%DCCPATH%DCC32.EXE" -LE. -B -JL fs%DACname%%IDEVer%.dpk %AddOptions%
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfs%DACname%%IDEVer%.dpk %AddOptions%
