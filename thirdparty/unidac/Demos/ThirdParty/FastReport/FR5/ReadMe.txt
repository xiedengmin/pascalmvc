Universal Data Access Components
Copyright 2008-2021, Devart. All Rights Reserved
--------------------------------------------------

Demo for FastReport included in UniDAC was built and tested using
Fast Report 5

Note:
Fast Query Builder of FastReport is disabled by default. To enable them you
should find frx.inc file in the FastReport installation directory and uncomment
following line:
//{$DEFINE QBUILDER}

IMPORTANT NOTE:
  Demo is provided as is, and there is no warranty that it is fully
  compatible with other versions of Fast Report.

Before using Demo you should install FastReport 5 UniDAC Components.
The following instruction will help you to compile and install 
FastReport 5 UniDAC Components manually using IDE.

C++Builder 2007
---------------
If you have only C++Builder 2007, you should perform the following steps

  1. Make sure that you have UniDAC and FastReport installed
  2. Open the Demos\Win32\ThirdParty\FastReport\FR5\Delphi11\MakeCBuilder.bat file in edit mode
  3. Make sure that the following paths are assigned correctly:
       DCCPATH - path to the compiler DCC32.exe
       PROJECTPATH - path to packages from Fast_Report_Demo\Delphi11
       FRLIBDLLPATH - path to FastReport5_Inst_Dir\LibD11
  4. Save MakeCBuilder.bat
  5. Run MakeCBuilder.bat. After that all generated files will appear in the project path (PROJECTPATH)
  6. Copy PROJECTPATH\*.bpl files to a folder that is included in the PATH environment variable
  7. Run IDE and add dclfrxUniDAC11.bpl and dclfsUniDAC11.bpl via Component->Install Packages... menu 
  8. To compile applications with UniDAC for FastReport components, add PROJECTPATH to the "Library Path" and "Include Path" lists

Delphi and C++Builder for Win32
--------------------------------

Run your IDE and walk through the following steps:
  1) Compile DAC run-time package (frxDACXX.dpk)
  2) Compile UniDAC run-time package (frxUniDACXX.dpk)
  3) Compile and install UniDAC design-time package (dclfrxUniDACXX.dpk)

You can find these packages in 
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi7\*.dpk - for Delphi 7 
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi9\*.dpk - for Delphi 2005
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi10\*.dpk - for Delphi 2006
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi11\*.dpk - for Delphi 2007
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi12\*.dpk - for Delphi 2009
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi14\*.dpk - for Delphi 2010
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi15\*.dpk - for Delphi XE
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi16\*.dpk - for Delphi XE2
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi17\*.dpk - for Delphi XE3
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi18\*.dpk - for Delphi XE4
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi19\*.dpk - for Delphi XE5
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi20\*.dpk - for Delphi XE6
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi21\*.dpk - for Delphi XE7
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi22\*.dpk - for Delphi XE8
  Demos\Win32\ThirdParty\FastReport\FR5\Delphi23\*.dpk - for Delphi 10 Seattle

To compile applications based on FastReport 5 UniDAC Components, add the following 
path to the "Library Path":
%UniDAC%\Demos\Win32\ThirdParty\FastReport\FR5\
where %UniDAC% is the UniDAC installation path on your computer.
