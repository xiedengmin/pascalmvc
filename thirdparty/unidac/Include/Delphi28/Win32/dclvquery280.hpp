// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'dclvquery280.dpk' rev: 35.00 (Windows)

#ifndef Dclvquery280HPP
#define Dclvquery280HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <VirtualQueryReg.hpp>
#include <VirtualQueryDesign.hpp>
#include <VirtualQueryEditor.hpp>
#include <VirtualMenu.hpp>
#include <VirtualAbout.hpp>
#include <VirtualQueryDesignUtils.hpp>
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <Data.SqlTimSt.hpp>	// (dbrtl)
#include <Data.FmtBcd.hpp>	// (dbrtl)
#include <Data.DB.hpp>	// (dbrtl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <CRFunctions.hpp>	// (dac280)
#include <CLRClasses.hpp>	// (dac280)
#include <CRParser.hpp>	// (dac280)
#include <MemUtils.hpp>	// (dac280)
#include <MemData.hpp>	// (dac280)
#include <MTSCall.hpp>	// (dac280)
#include <CRDataTypeMap.hpp>	// (dac280)
#include <CREncryption.hpp>	// (dac280)
#include <System.Win.ComObj.hpp>	// (rtl)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <CRXml.hpp>	// (dac280)
#include <CRServerEnumerator.hpp>	// (dac280)
#include <DBAccess.hpp>	// (dac280)
#include <CRVioSocket.hpp>	// (dac280)
#include <CRVioTcp.hpp>	// (dac280)
#include <DBMonitorClient.hpp>	// (dac280)
#include <DASQLMonitor.hpp>	// (dac280)
#include <CRConnectionPool.hpp>	// (dac280)
#include <CRAccess.hpp>	// (dac280)
#include <LiteConstsVirtual.hpp>	// (vquery280)
#include <LiteStaticVirtual.hpp>	// (vquery280)
#include <LiteCallVirtual.hpp>	// (vquery280)
#include <LiteFunctionVirtual.hpp>	// (vquery280)
#include <LiteCollationVirtual.hpp>	// (vquery280)
#include <LiteParserVirtual.hpp>	// (vquery280)
#include <LiteDataTypeMapVirtual.hpp>	// (vquery280)
#include <VirtualDataTypeMap.hpp>	// (vquery280)
#include <LiteClassesVirtual.hpp>	// (vquery280)
#include <CRVirtualData.hpp>	// (vquery280)
#include <VirtualTable.hpp>	// (dac280)
#include <System.Actions.hpp>	// (rtl)
#include <System.UIConsts.hpp>	// (rtl)
#include <Vcl.Graphics.hpp>	// (vcl)
#include <System.Messaging.hpp>	// (rtl)
#include <Vcl.ActnList.hpp>	// (vcl)
#include <System.HelpIntfs.hpp>	// (rtl)
#include <Winapi.UxTheme.hpp>	// (rtl)
#include <Vcl.GraphUtil.hpp>	// (vcl)
#include <Vcl.StdCtrls.hpp>	// (vcl)
#include <Vcl.Clipbrd.hpp>	// (vcl)
#include <Vcl.Printers.hpp>	// (vcl)
#include <Vcl.ComCtrls.hpp>	// (vcl)
#include <Vcl.Dialogs.hpp>	// (vcl)
#include <Vcl.ExtCtrls.hpp>	// (vcl)
#include <Vcl.Themes.hpp>	// (vcl)
#include <Winapi.FlatSB.hpp>	// (rtl)
#include <Vcl.Forms.hpp>	// (vcl)
#include <Vcl.Menus.hpp>	// (vcl)
#include <Winapi.MsCTF.hpp>	// (rtl)
#include <Vcl.Controls.hpp>	// (vcl)
#include <IDEMessages.hpp>	// (designide)
#include <Vcl.CaptionedDockTree.hpp>	// (vcl)
#include <Vcl.DockTabSet.hpp>	// (vcl)
#include <Vcl.Grids.hpp>	// (vcl)
#include <Vcl.CategoryButtons.hpp>	// (vcl)
#include <Vcl.ButtonGroup.hpp>	// (vcl)
#include <Vcl.SysStyles.hpp>	// (vcl)
#include <Winapi.D2D1.hpp>	// (rtl)
#include <Vcl.Direct2D.hpp>	// (vcl)
#include <Vcl.Styles.hpp>	// (vcl)
#include <Vcl.ImageCollection.hpp>	// (vclwinx)
#include <BrandingAPI.hpp>	// (designide)
#include <Winapi.GDIPOBJ.hpp>	// (rtl)
#include <Vcl.Buttons.hpp>	// (vcl)
#include <Vcl.Imaging.pngimage.hpp>	// (vclimg)
#include <Proxies.hpp>	// (designide)
#include <DesignEditors.hpp>	// (designide)
#include <System.Devices.hpp>	// (rtl)
#include <Vcl.AxCtrls.hpp>	// (vcl)
#include <Vcl.AppEvnts.hpp>	// (vcl)
#include <TreeIntf.hpp>	// (designide)
#include <TopLevels.hpp>	// (designide)
#include <StFilSys.hpp>	// (designide)
#include <IDEHelp.hpp>	// (designide)
#include <ComponentDesigner.hpp>	// (designide)
#include <PercentageDockTree.hpp>	// (designide)
#include <Vcl.WinXCtrls.hpp>	// (vclwinx)
#include <WaitDialog.hpp>	// (designide)
#include <Vcl.ExtDlgs.hpp>	// (vcl)
#include <Winapi.Mapi.hpp>	// (rtl)
#include <Vcl.ExtActns.hpp>	// (vcl)
#include <Vcl.ActnMenus.hpp>	// (vclactnband)
#include <Vcl.ActnMan.hpp>	// (vclactnband)
#include <Vcl.PlatformDefaultStyleActnCtrls.hpp>	// (vclactnband)
#include <BaseDock.hpp>	// (designide)
#include <DeskUtil.hpp>	// (designide)
#include <DeskForm.hpp>	// (designide)
#include <DockForm.hpp>	// (designide)
#include <Xml.Win.msxmldom.hpp>	// (xmlrtl)
#include <Xml.xmldom.hpp>	// (xmlrtl)
#include <ToolsAPI.hpp>	// (designide)
#include <DacReg.hpp>	// (dcldac280)
#include <VCLEditors.hpp>	// (designide)
#include <ToolWnds.hpp>	// (designide)
#include <IDEImageResources.hpp>	// (designide)
#include <ColnEdit.hpp>	// (designide)
#include <Vcl.DBLogDlg.hpp>	// (vcldb)
#include <Vcl.DBPWDlg.hpp>	// (vcldb)
#include <Vcl.DBCtrls.hpp>	// (vcldb)
#include <Vcl.DBGrids.hpp>	// (vcldb)
#include <CREditor.hpp>	// (dcldac280)
#include <DrpCtrls.hpp>	// (dcldb)
#include <DSDefine.hpp>	// (dcldb)
#include <DSDesign.hpp>	// (dcldb)
#include <CRDesign.hpp>	// (dcldac280)
#include <DacVcl.hpp>	// (dacvcl280)
#include <DAConnectionEditor.hpp>	// (dcldac280)
#include <DADesign.hpp>	// (dcldac280)
#include <CRSecureConnection.hpp>	// (dac280)
#include <CRHttp.hpp>	// (dac280)
#include <CRVersionChecker.hpp>	// (dcldac280)
// PRG_EXT: .bpl
// BPI_DIR: C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp
// OBJ_DIR: C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Dclvquery280
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dclvquery280 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DCLVQUERY280)
using namespace Dclvquery280;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Dclvquery280HPP
