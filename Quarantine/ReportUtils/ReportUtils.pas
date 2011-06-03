{******************************************************************************}
{*  Provides automatic report generation with Report Builder for any          *}
{*  TtiObjectList. For more information on Report Builder please visit the    *}
{*  Digital Metaphors web-site at http://www.digital-metaphors.com            *}
{*                                                                            *}
{*  Written by Andrew Denton (adenton@q-range.com  a_denton@blueyonder.co.uk )*}
{*  Version History :-                                                        *}
{*                                                                            *}
{*  1.0a 8th May 2002 - AD - Initial Version.                                 *}
{*  1.0b 17th May 2002 - AD - Added RemoveField method & a write method for   *}
{*                            SourceList which automatically gets the object  *}
{*                            properties.                                     *}
{*  1.0c 5th Aug  2003 - AD - Added underline to group header                 *}
{*  1.1a 28th Oct 2003 - AD - Added TtiVTListViewReporter                     *}
{*  1.2a 11th April 2007 - AD - Added ListviewPlusReporter                    *}
{*                              Modified for tiOPF2 and Report Builder 10     *}
{*                                                                            *}
{******************************************************************************}

Unit ReportUtils;

Interface

Uses
  SysUtils, TypInfo, Contnrs, tiObject, Classes, tiListView, tiListViewPlus,
  ppModule, daDataModule, ppDB, ppDBJIT, ppCache, ppClass, ppBands, ppComm,
  ppRelatv, ppProd, ppReport, ppVar, ppCtrls, ppPrnabl, ppTypes, ppViewr,
  tiVirtualTrees, tiVTListView;

Const
  fmtNoRTTI = 'No RTTI available for class %s.';

Type
  TGetTextEvent = Procedure(Sender : TObject; Var Text : String) Of Object;
  TMemoEvent = Procedure(Sender : TObject; aLines : TStrings) Of Object;

  TFieldInfo = Class
    fiPropInfo : TTypeInfo;
    fiFieldTitle : String;
    fiFieldName : String;
    fiWidth : Integer;
    fiRBDataType : TppDataType;
    fiGroupedField : Boolean;
  Public
    Constructor Create;
  End;

  TCustomReporter = Class
  Private
    FItemCount : Integer;
    FCurrXPos : Integer;
    FReportClass : TtiObject; // The class we are reporting on.
    FReport : TppReport;
    FPipeLine : TppJITPipeline;
    FRangeTitle : String;
    FReportTitle : String;
    FPropList : TObjectList;
    FReportSetup : Boolean;
    FIncludeCaption : Boolean;
    FRemoveGroup : Boolean;
    FFieldSpacing : Integer;
    FCurrentGroupValue : String;
    FOnGetGroupHeaderText : TGetTextEvent;
    FOnGetGroupFooterText : TGetTextEvent;
    FFieldGetText : TGetTextEvent;
    FMemoGetText : TMemoEvent;
    FLandscape : Boolean;
    FBorders : Boolean;
    FCurrLabelNo : Integer;
    FShowRecordCount : Boolean;
    FShowRecordNumber : Boolean;
    FCompanyName : String;
    Procedure SetLandscape(Const Value : Boolean);
  Protected
    Procedure ClearReport; Virtual;
    Procedure AssignClass; Virtual; Abstract;
    Procedure GetObjectProperties; Virtual;
    Function GetRBDataType(pObject : TObject; Const pProperty : String) : TppDataType;
    Procedure ClearBand(pBand : TppBand);
    Procedure RemoveColumnHeadings;
    Procedure SetupHeader; Virtual;
    Procedure SetupDetail; Virtual;
    Procedure SetupFooter; Virtual;
    Procedure DetailPrint(Sender : TObject);
    Function plCheckEOF : Boolean; Virtual; Abstract;
    Function plGetFieldValue(aFieldName : String) : Variant; Virtual; Abstract;
    Function GetReportFieldWidth(FieldType : TppDataType) : Integer; Virtual;
    Procedure AddLine(pBand : TppBand; pPos : Integer); Overload;
    Procedure AddLine(pBand : TppBand; pHPos, pVPos, pWidth : Integer); Overload;
    Procedure AddReportField(Const pFieldName : String; FieldNo : Integer;
      FieldType : TppDataType; pLastField : Boolean = False; pWidth : Integer = 0);
    Procedure AddColumnHeading(Const pHeading : String; FieldNo : Integer;
      FieldType : TppDataType; pWidth : Integer = 0);
    Procedure AddSubTotalField(pGroup : TppGroup; pBand : TppBand; pFieldName : String);
    Procedure AddATotalField(pBand : TppBand; pFieldName : String);
    Function FindReportField(Const pName : String) : TppComponent;
    Procedure GetGroupValue(Sender : TObject; Var pBreakValue : String);
    Procedure DefaultHeaderText(Sender : TObject; Var Text : String); Virtual;
    Procedure DefaultFooterText(Sender : TObject; Var Text : String); Virtual;
    Procedure GetFieldText(Sender : TObject; Var Text : String); Virtual;
    Procedure GetMemoText(Sender : TObject; aLines : TStrings); Virtual;
    Procedure PreviewFormCreate(Sender : TObject); Virtual;
    Function GetItemCount : Integer; Virtual; Abstract;
    Procedure SetReportTitle(Const Value : String);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure SetupReport;
    Procedure PrintTheReport(Const pPrintDest : String);
    Procedure PrintToDevices;
    Procedure AddHeaderLabel(X, Y : Integer; Const pText : String);
    Procedure AddGrouping(Const pGroupField, pTotalField : String; pAddHeaderBand, pAddFooterBand : Boolean); Overload;
    Procedure AddGrouping(Const pGroupField : String; pTotalFields : TStringList; pAddHeaderBand, pAddFooterBand : Boolean); Overload;
    Procedure AddTotalField(Const pTotalField : String);
    Procedure AddTotalFields(pTotalFields : TStringList);
    Procedure SetFieldTitle(Const pFNumber : Integer; Const pTitle : String);
    Procedure RemoveField(Const pFieldName : String);
  Published
    Property Report : TppReport Read FReport Write FReport;
    Property LandscapePrint : Boolean Read FLandscape Write SetLandscape;
    Property FieldList : TObjectList Read FPropList Write FPropList;
    Property CompanyName : String Read FCompanyName Write FCompanyName;
    Property ReportTitle : String Read FReportTitle Write SetReportTitle;
    Property RangeTitle : String Read FRangeTitle Write FRangeTitle;
    Property IncludeCaption : Boolean Read FIncludeCaption Write FIncludeCaption;
    Property RemoveGroupItemFromReport : Boolean Read FRemoveGroup Write FRemoveGroup;
    Property FieldSpacing : Integer Read FFieldSpacing Write FFieldSpacing;
    Property CurrentGroupValue : String Read FCurrentGroupValue Write FCurrentGroupValue;
    Property ItemCount : Integer Read GetItemCount Write FItemCount;
    Property BorderedFields : Boolean Read FBorders Write FBorders;
    Property ShowRecordNumber : Boolean Read FShowRecordNumber Write FShowRecordNumber;
    Property ShowRecordCount : Boolean Read FShowRecordCount Write FShowRecordCount;
    {Events}
    Property OnGetGroupHeaderText : TGetTextEvent Read FOnGetGroupHeaderText Write FOnGetGroupHeaderText;
    Property OnGetGroupFooterText : TGetTextEvent Read FOnGetGroupFooterText Write FOnGetGroupFooterText;
    Property FieldGetText : TGetTextEvent Read FFieldGetText Write FFieldGetText;
    Property MemoGetText : TMemoEvent Read FMemoGetText Write FMemoGetText;
  End;

  TtiObjectListReporter = Class(TCustomReporter)
  Private
    FSourceList : TtiObjectList;
  Protected
    Function plCheckEOF : Boolean; Override;
    Procedure AssignClass; Override;
    Procedure SetSourceList(Const Value : TtiObjectList);
    Function plGetFieldValue(aFieldName : String) : Variant; Override;
    Function GetItemCount : Integer; Override;
  Public
    Constructor Create;
  Published
    Property SourceList : TtiObjectList Read FSourceList Write SetSourceList;
  End;

  TListViewReporter = Class(TCustomReporter)
  Private
    FListView : TtiCustomListView;
  Protected
    Procedure AssignClass; Override;
    Procedure GetObjectProperties; Override;
    Function plCheckEOF : Boolean; Override;
    Function plGetFieldValue(aFieldName : String) : Variant; Override;
    Function GetItemCount : Integer; Override;
    Procedure SetListView(pLV : TtiCustomListView);
  Public
    Constructor Create;
  Published
    Property ListView : TtiCustomListView Read FListView Write SetListView;
  End;

  TVtListViewReporter = Class(TCustomReporter)
  Private
    FListView : TtiVTListView;
    //FCurrentNode : PVirtualNode;
    FCurrentObject : TtiObject;
    FLastIndex : Integer;
  Protected
    Procedure AssignClass; Override;
    Procedure GetObjectProperties; Override;
    Procedure PreviewFormCreate(Sender : TObject); Override;
    Function plCheckEOF : Boolean; Override;
    Function plGetFieldValue(aFieldName : String) : Variant; Override;
    Function GetItemCount : Integer; Override;
    Function GetNodeNumber(pNodeNo : Integer) : PVirtualNode;
    Procedure SetListView(pLV : TtiVTListView);
  Public
    Constructor Create;
  Published
    Property ListView : TtiVTListView Read FListView Write SetListView;
  End;

  TListViewPlusReporter = Class(TListViewReporter)
  Private
    FListViewPlus : TtiListViewPlus;
    Procedure SetListViewPlus(Const Value : TtiListViewPlus);
  Protected
    Procedure AssignClass; Override;
    Procedure GetObjectProperties; Override;
    Function plCheckEOF : Boolean; Override;
    Function plGetFieldValue(aFieldName : String) : Variant; Override;
    Function GetItemCount : Integer; Override;
  Published
    Property ListViewPlus : TtiListViewPlus Read FListViewPlus Write SetListViewPlus;
  End;

  ENoRunTimeTypeInfo = Class(Exception);
  EEmptyPerObjList = Class(Exception);

Procedure ShowMaximised(Const pReport : TppReport);

Implementation

Uses Dialogs, Graphics, Forms, tiLog, Printers, ppMemo, StdStuff;


Procedure ShowMaximised(Const pReport : TppReport);
Begin
  pReport.PreviewForm.WindowState := wsMaximized;
  TppViewer(pReport.PreviewForm.Viewer).ZoomSetting := zs100Percent;
End;

{ TCustomReporter }

Procedure TCustomReporter.AddColumnHeading(Const pHeading : String;
  FieldNo : Integer; FieldType : TppDataType; pWidth : Integer = 0);
Var
  lblHeading : TppLabel;
Begin
  lblHeading := TppLabel.Create(Nil);
  lblHeading.Name := 'lblHeading' + IntToStr(FieldNo);
  lblHeading.Band := FReport.HeaderBand;
  lblHeading.spLeft := FCurrXPos;
  lblHeading.spTop := FReport.HeaderBand.spHeight - 16;
  lblHeading.AutoSize := True;
  lblHeading.Caption := pHeading;
  lblHeading.Font.Style := [fsBold];
  If (pWidth <> 0) Then
    lblHeading.spWidth := pWidth
  Else
    lblHeading.spWidth := GetReportFieldWidth(FieldType);
  If FieldType In [dtCurrency, dtInteger, dtDouble, dtSingle, dtExtended] Then
    lblHeading.Alignment := taRightJustify;
End;

Procedure TCustomReporter.AddGrouping(Const pGroupField, pTotalField : String;
  pAddHeaderBand, pAddFooterBand : Boolean);
Var
  lGroupLabel : TppLabel;
  lGroup : TppGroup;
  lGroupHeader : TppGroupHeaderBand;
  lGroupFooter : TppGroupFooterBand;
  I : Integer;
Begin
  If Not FReportSetup Then
    SetupReport;
  For I := 0 To FPropList.Count - 1 Do
  Begin
    If (TFieldInfo(FPropList.Items[I]).fiFieldName = pGroupField) Then
    Begin
      TFieldInfo(FPropList.Items[I]).fiGroupedField := True;
      Break;
    End;
  End; { Loop }
  ClearBand(FReport.DetailBand);
  RemoveColumnHeadings;
  FCurrXPos := 0;
  SetupDetail;
  lGroup := TppGroup.Create(Nil);
  lGroup.Report := FReport;
  lGroup.BreakType := btDataField;
  lGroup.DataPipeline := FPipeLine;
  lGroup.BreakName := pGroupField;
  lGroup.OnGetBreakValue := GetGroupValue;
  If pAddHeaderBand Then
  Begin
    lGroupHeader := TppGroupHeaderBand.Create(Nil);
    lGroupHeader.Group := lGroup;
    lGroupHeader.Height := 8;
    lGroupLabel := TppLabel.Create(Nil);
    lGroupLabel.Band := lGroupHeader;
    lGroupLabel.spTop := 2;
    lGroupLabel.AutoSize := True;
    lGroupLabel.spLeft := 2;
    lGroupLabel.OnGetText := OnGetGroupHeaderText;
    AddLine(lGroupHeader, 2, 12, 90);
  End;
  If pAddFooterBand Then
  Begin
    lGroupFooter := TppGroupFooterBand.Create(Nil);
    lGroupFooter.Group := lGroup;
    lGroupFooter.Height := 8;
    lGroupLabel := TppLabel.Create(Nil);
    lGroupLabel.Band := lGroupFooter;
    lGroupLabel.spTop := 2;
    lGroupLabel.AutoSize := True;
    lGroupLabel.spLeft := 2;
    lGroupLabel.OnGetText := OnGetGroupFooterText;
    AddSubTotalField(lGroup, lGroupFooter, pTotalField);
  End;
End;

Procedure TCustomReporter.AddGrouping(Const pGroupField : String;
  pTotalFields : TStringList; pAddHeaderBand, pAddFooterBand : Boolean);
Var
  I : Integer;
  lGroup : TppGroup;
  lGroupHeader : TppGroupHeaderBand;
  lGroupFooter : TppGroupFooterBand;
  lGroupLabel : TppLabel;
Begin
  If Not FReportSetup Then
    SetupReport;
  For I := 0 To FPropList.Count - 1 Do
  Begin
    If (TFieldInfo(FPropList.Items[I]).fiFieldName = pGroupField) Then
    Begin
      TFieldInfo(FPropList.Items[I]).fiGroupedField := True;
      Break;
    End;
  End; { Loop }
  ClearBand(FReport.DetailBand);
  RemoveColumnHeadings;
  SetupDetail;
  lGroup := TppGroup.Create(Nil);
  lGroup.Report := FReport;
  lGroup.BreakType := btDataField;
  lGroup.DataPipeline := FPipeLine;
  lGroup.BreakName := pGroupField;
  lGroup.OnGetBreakValue := GetGroupValue;
  If pAddHeaderBand Then
  Begin
    lGroupHeader := TppGroupHeaderBand.Create(Nil);
    lGroupHeader.Group := lGroup;
    lGroupHeader.Height := 8;
    lGroupLabel := TppLabel.Create(Nil);
    lGroupLabel.Band := lGroupHeader;
    lGroupLabel.spTop := 2;
    lGroupLabel.AutoSize := True;
    lGroupLabel.spLeft := 2;
    lGroupLabel.OnGetText := OnGetGroupHeaderText;
    AddLine(lGroupHeader, 2, 14, 80);
  End;
  If pAddFooterBand Then
  Begin
    lGroupFooter := TppGroupFooterBand.Create(Nil);
    lGroupFooter.Group := lGroup;
    lGroupFooter.Height := 8;
    lGroupLabel := TppLabel.Create(Nil);
    lGroupLabel.Band := lGroupFooter;
    lGroupLabel.spTop := 2;
    lGroupLabel.AutoSize := True;
    lGroupLabel.spLeft := 2;
    lGroupLabel.OnGetText := OnGetGroupFooterText;
    For I := 0 To pTotalFields.Count - 1 Do
      AddSubTotalField(lGroup, lGroupFooter, pTotalFields.Strings[I]);
  End;
End;

Procedure TCustomReporter.AddLine(pBand : TppBand; pHPos, pVPos, pWidth : Integer);
Var
  lLine : TppLine;
Begin
  lLine := TppLine.Create(Nil);
  lLine.Band := pBand;
  lLine.Pen.Width := 2;
  lLine.spWidth := pWidth;
  lLine.spLeft := pHPos;
  lLine.spTop := pVPos;
End;

Procedure TCustomReporter.AddLine(pBand : TppBand; pPos : Integer);
Var
  lLine : TppLine;
Begin
  lLine := TppLine.Create(Nil);
  lLine.Band := pBand;
  lLine.ParentWidth := True;
  lLine.spTop := pPos;
End;

Procedure TCustomReporter.AddReportField(Const pFieldName : String;
  FieldNo : Integer; FieldType : TppDataType; pLastField : Boolean = False; pWidth : Integer = 0);
Var
  lblField : TppDBText;
  lblMemo : TppDBMemo;
Begin
//  Log('Adding field at position %d.',[FCurrXPos]);
  If (FieldType = dtString) Then
  Begin
    lblMemo := TppDBMemo.Create(Nil);
    lblMemo.Name := pFieldName;
    lblMemo.Band := FReport.DetailBand;
    lblMemo.spLeft := FCurrXPos;
    lblMemo.spTop := 2;
    lblMemo.DataPipeline := FPipeLine;
    lblMemo.AutoSize := Not FBorders;
    lblMemo.WordWrap := True;
    lblMemo.Stretch := True;
    lblMemo.Transparent := True;
    lblMemo.DataField := pFieldName;
    If FBorders Then
    Begin
      lblMemo.Border.All := True;
      lblMemo.Border.Visible := True;
    End;
    //lblMemo.OnGetMemo := GetMemoText;
    If Not pLastField Then
    Begin
      If (pWidth = 0) Then
        lblMemo.spWidth := GetReportFieldWidth(FieldType)
      Else
        lblMemo.spWidth := pWidth;
    End
    Else
      lblMemo.spWidth := FReport.DetailBand.spWidth - FCurrXPos;
    FCurrXPos := FCurrXPos + lblMemo.spWidth + FieldSpacing;
  End
  Else
  Begin
    lblField := TppDBText.Create(Nil);
    lblField.Name := pFieldName;
    lblField.Band := FReport.DetailBand;
    lblField.spLeft := FCurrXPos;
    lblField.spTop := 2;
    lblField.Transparent := True;
    If FBorders Then
    Begin
      lblField.Border.All := True;
      lblField.Border.Visible := True;
    End;
    lblField.DataPipeline := FPipeLine;
    lblField.AutoSize := Not FBorders;
    lblField.WordWrap := True;
    lblField.DataField := pFieldName;
    lblField.OnGetText := GetFieldText;
    If (FieldType = dtExtended) Then
      lblField.DisplayFormat := '#,0.00';
    If (FieldType In [dtCurrency, dtInteger, dtSingle, dtDouble, dtExtended]) Then
      lblField.Alignment := taRightJustify;
    If Not pLastField Then
    Begin
      If (pWidth = 0) Then
        lblField.spWidth := GetReportFieldWidth(FieldType)
      Else
        lblField.spWidth := pWidth;
    End
    Else
    Begin
      If (FieldType = dtString) Then // Only expand string fields as numbers look silly.
        lblField.spWidth := FReport.DetailBand.spWidth - FCurrXPos
      Else
        lblField.spWidth := pWidth;
    End;
    FCurrXPos := FCurrXPos + lblField.spWidth + FieldSpacing;
  End;
End;

Procedure TCustomReporter.AddSubTotalField(pGroup : TppGroup; pBand : TppBand; pFieldName : String);
Var
  lTotalField : TppDBCalc;
  lTotalFieldNo : Integer;
  lTempDataField : TppField;
  lTempField : TppComponent;
  lLineTop,
    lLineLeft,
    lLineWidth : Integer;
Begin
  lLineWidth := 0;
  lLineLeft := 0;
  lLineTop := 0;
  lTotalField := TppDBCalc.Create(Nil);
  lTotalField.Band := pBand;
  lTotalField.AutoSize := False;
  lTotalField.Alignment := taRightJustify;
  lTotalField.ResetGroup := pGroup;
  lTotalField.DBCalcType := dcSum;
  lTotalField.spTop := 6;
  lTotalField.DataPipeline := FPipeLine;
  lTotalField.DataField := pFieldName;
  lTotalFieldNo := FPipeLine.IndexOfFieldName(pFieldName);
  lTempField := FindReportField(pFieldName);
  If (lTempField <> Nil) Then // Should never fail, but you never know !
  Begin
    lTotalField.spLeft := lTempField.spLeft; // Align total field with item.
    lLineWidth := lTempField.spWidth; // Get total line dimensions & coordinates.
    lLineLeft := lTempField.spLeft;
    lLineTop := 4;
  End;
  If (lTotalFieldNo > -1) Then
  Begin
    lTempDataField := FPipeLine.Fields[lTotalFieldNo];
    lTotalField.spWidth := GetReportFieldWidth(lTempDataField.DataType);
    If (lTempDataField.DataType = dtCurrency) Then
      lTotalField.DisplayFormat := CurrencyString + '0#.00';
  End;
  AddLine(pBand, lLineLeft, lLineTop, lLineWidth);
  AddATotalField(FReport.SummaryBand, pFieldName);
End;

Procedure TCustomReporter.AddATotalField(pBand : TppBand; pFieldName : String);
Var
  lTotalField : TppDBCalc;
  lTotalFieldNo : Integer;
  lTempDataField : TppField;
  lTempField : TppComponent;
  lLineTop,
    lLineLeft,
    lLineWidth : Integer;
Begin
  lLineWidth := 0;
  lLineLeft := 0;
  lLineTop := 0;
  lTotalField := TppDBCalc.Create(Nil);
  lTotalField.Band := pBand;
  lTotalField.AutoSize := False;
  lTotalField.Alignment := taRightJustify;
  lTotalField.DBCalcType := dcSum;
  lTotalField.spTop := 5;
  lTotalField.DataPipeline := FPipeLine;
  lTotalField.DataField := pFieldName;
  lTotalFieldNo := FPipeLine.IndexOfFieldName(pFieldName);
  lTempField := FindReportField(pFieldName);
  If (lTempField <> Nil) Then // Should never fail, but you never know !
  Begin
    lTotalField.spLeft := lTempField.spLeft; // Align total field with item.
    lLineWidth := lTempField.spWidth; // Get total line dimensions & coordinates.
    lLineLeft := lTempField.spLeft;
    lLineTop := lTempField.spTop - 1;
  End;
  If (lTotalFieldNo > -1) Then
  Begin
    lTempDataField := FPipeLine.Fields[lTotalFieldNo];
    lTotalField.spWidth := GetReportFieldWidth(lTempDataField.DataType);
    Case lTempDataField.DataType Of
      dtCurrency : lTotalField.DisplayFormat := CurrencyString + '0#.00';
      dtTime : Begin
                 lTotalField.DisplayFormat := 'hh:nn:ss';
                 lTotalField.Alignment := taLeftJustify;
               End;
    End; { Case }
  End;
  AddLine(pBand, lLineLeft, lLineTop, lLineWidth);
End;

Procedure TCustomReporter.AddTotalField(Const pTotalField : String);
Begin
  If Not FReportSetup Then
    SetupReport;
  AddATotalField(FReport.SummaryBand, pTotalField);
End;

Procedure TCustomReporter.AddTotalFields(pTotalFields : TStringList);
Var
  I : Integer;
Begin
  For I := 0 To pTotalFields.Count - 1 Do
    AddTotalField(pTotalFields.Strings[I]);
End;

Procedure TCustomReporter.ClearBand(pBand : TppBand);
Var
  I, O : Integer;
Begin
  O := pBand.ObjectCount;
  For I := O - 1 Downto 0 Do
    pBand.Objects[I].Destroy;
End;

Constructor TCustomReporter.Create;
Begin
  FReport := TppReport.Create(Nil);
  FReport.Units := utMillimeters;
  FReport.DeviceType := 'Printer';
  FPipeLine := TppJITPipeLine.Create(Nil);
  FPipeLine.RecordCount := GetItemCount;
  FPipeLine.MoveBy := 1;
   // LogFmt('Pipeline Record count set to %d.', [FPipeLine.RecordCount]);
  // Hook up the event handlers for the pipeline.
  FPipeLine.OnCheckEOF := plCheckEOF;
  FPipeLine.OnGetFieldValue := plGetFieldValue;

  FReport.DataPipeline := FPipeLine;
  FReport.DefaultBands := FReport.DefaultBands + [btSummary];
  FReport.CreateDefaultBands;
  FReport.DetailBand.spHeight := 12;
  FReport.HeaderBand.Height := 24.0;
  FReport.SummaryBand.Height := 8.5;
  FReport.PrinterSetup.PaperName := 'A4';
  //FReport.PrinterSetup.Orientation :=  poLandscape;
  FReport.OutlineSettings.Visible := False;
  FReport.OnPreviewFormCreate := PreviewFormCreate;
  FReport.AllowPrintToFile := True;
  FReport.TextFileName := '';
  FReport.PassSetting := psTwoPass;
  FPropList := TObjectList.Create;
  FFieldSpacing := 5; // Default field spacing in report units.
  FReportSetup := False;
  FIncludeCaption := False;
  FRemoveGroup := True;
  FRangeTitle := 'All Items';
  FOnGetGroupHeaderText := DefaultHeaderText; //Nil;
  FOnGetGroupFooterText := DefaultFooterText; //Nil;
  FMemoGetText := GetMemoText;
  FFieldGetText := GetFieldText;
  FBorders := False;
  FShowRecordCount := True;
  FShowRecordNumber := False;
End;

Procedure TCustomReporter.DefaultFooterText(Sender : TObject; Var Text : String);
Begin
  Text := 'Footer for ' + FCurrentGroupValue;
End;

Procedure TCustomReporter.DefaultHeaderText(Sender : TObject; Var Text : String);
Begin
  Text := 'Header for ' + FCurrentGroupValue;
End;

Destructor TCustomReporter.Destroy;
Begin
  FPropList.Free;
  FPipeLine.Free;
  FReport.FreeBandsAndGroups;
  FReport.Free;
End;

Function TCustomReporter.FindReportField(Const pName : String) : TppComponent;
Var
  I : Integer;
  lObject : TppComponent;
Begin
  Result := Nil;
  For I := 0 To FReport.DetailBand.ObjectCount - 1 Do
  Begin
    lObject := FReport.DetailBand.Objects[I];
    If (CompareText(pName, lObject.Name) = 0) Then
    Begin
      Result := lObject;
      Break;
    End;
  End; { Loop }
End;

Procedure TCustomReporter.GetGroupValue(Sender : TObject; Var pBreakValue : String);
Begin
  FCurrentGroupValue := pBreakValue;
End;

Procedure TtiObjectListReporter.AssignClass;
Begin
  FReportClass := FSourceList.Items[0];
End;

Constructor TtiObjectListReporter.Create;
Begin
  Inherited;
  FPipeLine.InitialIndex := 0;
  FPipeLine.OnCheckEOF := Nil;
End;

Function TtiObjectListReporter.GetItemCount : Integer;
Begin
  If (FSourceList <> Nil) Then
    Result := FSourceList.Count
  Else
    Result := 0;
End;

Procedure TCustomReporter.GetObjectProperties;
Var
  ThisProperty : Integer;
  PropertyCount : Integer;
  PropertyList : PPropList;
  lFieldInfo : TFieldInfo;
Begin
  AssignClass;
  If (ItemCount > 0) Then
  Begin
    FPropList.Clear;
    If (FReportClass.ClassInfo = Nil) Then
      Raise ENoRunTimeTypeInfo.CreateFmt(fmtNoRTTI, [FReportClass.ClassName]);
    // Get the number of properties the object has.
    PropertyCount := GetTypeData(FReportClass.ClassInfo)^.PropCount;
    // Iterate through the properties collecting their names and types.
    If (PropertyCount > 0) Then
    Begin
      GetMem(PropertyList, PropertyCount * SizeOf(Pointer));
      Try
        GetPropInfos(FReportClass.ClassInfo, PropertyList);
        For ThisProperty := 0 To PropertyCount - 1 Do
        Begin
          //Don't worry about freeing lFieldInfo as ObjectList will do this for us.
          If (PropertyList^[ThisProperty]^.Name <> 'Caption') Or FIncludeCaption Then
          Begin
            lFieldInfo := TFieldInfo.Create;
            lFieldInfo.fiPropInfo := PropertyList^[ThisProperty]^.PropType^^;
            lFieldInfo.fiFieldTitle := PropertyList^[ThisProperty]^.Name;
            lFieldInfo.fiFieldName := PropertyList^[ThisProperty]^.Name;
            lFieldInfo.fiRBDataType := GetRBDataType(FReportClass, PropertyList^[ThisProperty]^.Name);
            FPropList.Add(lFieldInfo);
          End;
        End; { Loop }
      Finally
        FreeMem(PropertyList, PropertyCount * SizeOf(Pointer));
      End;
    End;
  End
  Else
    Raise EEmptyPerObjList.Create('SourceList is empty!');
End;

Function TCustomReporter.GetRBDataType(pObject : TObject; Const pProperty : String) : TppDataType;
Var
  PropertyPtr : PPropInfo;
  lDateTest : PPropInfo;
  lDateInfo : Pointer;

  Function PropertyKind(pObject : TObject; Const pProperty : String) : TTypeKind;
  Begin
    Result := tkUnknown; //default
  { Get reference to property info.. }
    PropertyPtr := GetPropInfo(pObject.ClassInfo, pProperty);
    If (PropertyPtr <> Nil) Then //return property type
      Result := PropertyPtr^.PropType^.Kind;
  End;

Begin
  Result := dtNotknown;
  { Determine property type and return Report Builder data type accordingly..}
  Case PropertyKind(pObject, pProperty) Of
    tkClass,
      tkString,
      tkLString,
      tkWString,
      tkEnumeration : Result := dtString;
    tkInt64,
      tkInteger : Result := dtInteger;
    tkChar : Result := dtChar;
    tkFloat :
      Begin
        If (GetTypeData(PropertyPtr.PropType^)^.FloatType = ftCurr) Then
          Result := dtCurrency
        Else
        Begin
          lDateInfo := TypeInfo(TDateTime);
          lDateTest := GetPropInfo(pObject, pProperty);
          If (lDateInfo = lDateTest^.PropType^) Then
            Result := dtDateTime
          Else
            Result := dtExtended;
        End;
      End;
    tkVariant : Result := dtVariant;
    tkUnknown : Result := dtString;
  End; { Case }
End;

Function TCustomReporter.GetReportFieldWidth(FieldType : TppDataType) : Integer;
Begin
  Result := 100;
  Case FieldType Of
    dtDateTime : Result := 100;
    dtInteger : Result := 50;
    dtString : Result := 120;
    dtSingle,
      dtDouble,
      dtExtended : Result := 60;
    dtCurrency : Result := 60;
  End; { Case }
End;

Function TtiObjectListReporter.plCheckEOF : Boolean;
Begin
  Result := False;
End;

Function TtiObjectListReporter.plGetFieldValue(aFieldName : String) : Variant;
Var
  lData : TtiObject;
Begin
  Try
    If aFieldName = 'RECNO' Then
      Result := FPipeLine.RecordIndex + 1
    Else
    Begin
      lData := SourceList.Items[FPipeLine.RecordIndex];
      Result := GetPropValue(lData, aFieldName);
    End;
  Except
    Result := 'Error.';
  End;
End;

Procedure TCustomReporter.PrintTheReport(Const pPrintDest : String);
Begin
  If Not FReportSetup Then
    SetupReport;
  FReport.DeviceType := pPrintDest;
  FReport.PrintReport;
End;

Procedure TCustomReporter.RemoveColumnHeadings;
Var
  I : Integer;
Begin
  With FReport.HeaderBand Do
  Begin
    For I := ObjectCount - 1 Downto 0 Do
    Begin
      If (Copy(Objects[I].Name, 1, 10) = 'lblHeading') Then
        Objects[I].Destroy;
    End;
  End;
End;

Procedure TCustomReporter.SetFieldTitle(Const pFNumber : Integer; Const pTitle : String);
Begin
  Try
    TFieldInfo(FPropList.Items[pFNumber]).fiFieldTitle := pTitle;
  Except
    On E : Exception Do
      ShowMessage(Format('Error %s setting field title %d', [E.Message, pFNumber]));
  End;
End;

Procedure TCustomReporter.SetupDetail;
Var
  I : Integer;
  lBand : TppShape;
  lFieldInfo : TFieldInfo;
Begin
  lBand := TppShape.Create(FReport.DetailBand);
  lBand.ParentHeight := True;
  lBand.ParentWidth := True;
  lBand.Pen.Style := psClear;
  lBand.OnPrint := DetailPrint;
  lBand.StretchWithParent := True;
  lBand.UserName := 'shpDetail';
  lBand.Brush.Color := clInfoBk;
  lBand.Band := FReport.DetailBand;
  lBand.SendToBack;
  FCurrXPos := 0;
  If ShowRecordNumber Then
  Begin
    lFieldInfo := TFieldInfo.Create;
    lFieldInfo.fiFieldTitle := '#';
    lFieldInfo.fiFieldName := 'RECNO';
    lFieldInfo.fiWidth := 50;
    lFieldInfo.fiRBDataType := dtInteger;
    FPropList.Insert(0, lFieldInfo);
  End;
  For I := 0 To FPropList.Count - 1 Do
  Begin
    With TFieldInfo(FPropList.Items[I]) Do
    Begin
      If (FPipeLine.IndexOfFieldName(fiFieldName) = -1) Then
        FPipeLine.DefineField(fiFieldName, fiRBDataType, 25);
      If Not fiGroupedField Or Not FRemoveGroup Then
      Begin
        AddColumnHeading(fiFieldTitle, I, fiRBDataType, fiWidth);
        AddReportField(fiFieldName, I, fiRBDataType, (I = Pred(FPropList.Count)), fiWidth);
//        Log('Adding field %d "%s" with a width of %d', [I, fiFieldName, fiWidth]);
      End;
    End;
  End; { Loop }
End;

Procedure TCustomReporter.SetupFooter;
Var
  EOPLabel : TppLabel;
  RecordLabel : TppLabel;
Begin
  AddLine(FReport.SummaryBand, 22);
  If ShowRecordCount Then
  Begin
    RecordLabel := TppLabel.Create(Nil);
    RecordLabel.Band := FReport.SummaryBand;
    RecordLabel.Font.Style := [fsBold];
    RecordLabel.Caption := Format('%d Item(s) Reported', [ItemCount]);
    RecordLabel.AutoSize := True;
    RecordLabel.spLeft := 2;
    RecordLabel.spTop := 4;
  End;
  EOPLabel := TppLabel.Create(Nil);
  EOPLabel.Band := FReport.SummaryBand;
  EOPLabel.Autosize := True;
  EOPLabel.Font.Style := [fsBold];
  EOPLabel.Caption := '- - End Of Report - -';
  EOPLabel.Alignment := taCenter;
  EOPLabel.spLeft := (FReport.PrinterSetup.PageDef.spPrintableWidth -
    EOPLabel.spWidth) Div 2;
  EOPLabel.spTop := 26;
End;

Procedure TCustomReporter.SetupHeader;
Var
  lSysVar : TppSystemVariable;
  lCompanyName,
    lRangeTitle,
    lReportTitle : TppLabel;
  lShape : TppShape;
Begin
  lCompanyName := TppLabel.Create(Nil);
  lCompanyName.spLeft := 2;
  lCompanyName.spTop := 2;
  lCompanyName.Band := FReport.HeaderBand;
  lCompanyName.AutoSize := True;
  lCompanyName.Font.Size := 14;
  lCompanyName.Font.Style := [fsBold];
  lCompanyName.Font.Color := clSkyBlue;
  lCompanyName.Caption := FCompanyName;

  lReportTitle := TppLabel.Create(Nil);
  lReportTitle.Band := FReport.HeaderBand;
  lReportTitle.spLeft := 2;
  lReportTitle.spTop := 24;
  lReportTitle.AutoSize := True;
  lReportTitle.Caption := FReportTitle;
  lReportTitle.Font.Size := 12;
  lReportTitle.Font.Style := [fsBold];
  lReportTitle.Font.Color := clGray;

  lRangeTitle := TppLabel.Create(Nil);
  lRangeTitle.Band := FReport.HeaderBand;
  lRangeTitle.spLeft := 2;
  lRangeTitle.spTop := 42;
  lRangeTitle.AutoSize := True;
  lRangeTitle.Caption := FRangeTitle;
  lRangeTitle.Font.Style := [fsBold];

  lSysVar := TppSystemVariable.Create(Nil);
  lSysVar.VarType := vtPrintDateTime;
  lSysVar.spTop := FReport.HeaderBand.spHeight - 30;
  lSysVar.spLeft := 2;
  lSysVar.DisplayFormat := 'mmmm d, yyyy hh:nn:ss';
  lSysVar.Font.Style := [fsBold];
  lSysVar.Band := FReport.HeaderBand;

  lSysVar.Band := FReport.HeaderBand;
  lSysVar := TppSystemVariable.Create(Nil);
  lSysVar.VarType := vtPageSetDesc;
  lSysVar.Band := FReport.HeaderBand;
  lSysVar.Alignment := taRightJustify;
  lSysVar.spLeft := (FReport.PrinterSetup.PageDef.spPrintableWidth -
    lSysVar.spWidth) - 2;
  lSysVar.spTop := FReport.HeaderBand.spHeight - 30;
  lSysVar.Font.Style := [fsBold];

  lShape := TppShape.Create(Nil);
  lShape.Band := FReport.HeaderBand;
  lShape.ParentWidth := True;
  lShape.spTop := FReport.HeaderBand.spHeight - 18;
  lShape.spHeight := 17;
End;

Procedure TCustomReporter.SetupReport;
Begin
  ClearReport;
  SetupHeader;
  SetupDetail;
  SetupFooter;
  FReportSetup := True;
End;

Procedure TCustomReporter.RemoveField(Const pFieldName : String);
Var
  I : Integer;
  lInfo : TFieldInfo;
Begin
  For I := 0 To FPropList.Count - 1 Do
  Begin
    lInfo := TFieldInfo(FPropList.Items[I]);
    If (CompareText(lInfo.fiFieldName, pFieldName) = 0) Then
    Begin
      FPropList.Delete(I);
      Break;
    End;
  End; { Loop }
End;

Procedure TCustomReporter.PreviewFormCreate(Sender : TObject);
Begin
  ShowMaximised(FReport);
End;

Procedure TCustomReporter.ClearReport;
Begin
  ClearBand(FReport.HeaderBand);
  ClearBand(FReport.DetailBand);
  ClearBand(FReport.SummaryBand);
  FReportSetup := False;
  FCurrXPos := 0;
End;

Procedure TCustomReporter.PrintToDevices;
Begin
  If (GetItemCount > 0) Then
  Begin
    If Not FReportSetup Then
      SetupReport;
    FReport.PrintToDevices;
  End
  Else
    ErrorBox('Nothing to print!');
End;

Procedure TCustomReporter.SetLandscape(Const Value : Boolean);
Begin
  FLandscape := Value;
  If FLandscape Then
    FReport.PrinterSetup.Orientation := poLandscape
  Else
    FReport.PrinterSetup.Orientation := poPortrait;
End;

Procedure TCustomReporter.GetFieldText(Sender : TObject; Var Text : String);
Begin
//  Log('Field text is ' + Text);
  If (Trim(Text) = '') Then
    Text := '-'
  Else If Trim(Text) = '30/12/1899' Then
    Text := '-';
End;

Procedure TCustomReporter.GetMemoText(Sender : TObject; aLines : TStrings);
Begin
  {If (Trim(aLines.Text) = '') Then
    aLines.Text := '-'; }
End;

Procedure TCustomReporter.DetailPrint(Sender : TObject);
Begin
  If Odd(FPipeLine.RecordIndex) Then
    (Sender As TppShape).Brush.Color := clInfoBk
  Else
    (Sender As TppShape).Brush.Color := clWhite;
End;

Procedure TCustomReporter.SetReportTitle(Const Value : String);
Begin
  FReportTitle := Value;
  FReport.TextFileName := Value + '.Txt';
End;

Procedure TCustomReporter.AddHeaderLabel(X, Y : Integer; Const pText : String);
Var
  lblHeading : TppLabel;
Begin
  lblHeading := TppLabel.Create(Nil);
  lblHeading.Name := 'lblHeading' + IntToStr(FCurrLabelNo);
  lblHeading.Band := FReport.HeaderBand;
  lblHeading.spLeft := X;
  lblHeading.spTop := Y;
  lblHeading.AutoSize := True;
  lblHeading.Caption := pText;
  Inc(FCurrLabelNo);
  lblHeading.Font.Style := [fsBold];
End;

{ TFieldInfo }

Constructor TFieldInfo.Create;
Begin
  fiGroupedField := False;
End;

{ TtiObjectListReporter }

Procedure TtiObjectListReporter.SetSourceList(Const Value : TtiObjectList);
Begin
  FSourceList := Value;
  FPipeLine.RecordCount := GetItemCount;
  GetObjectProperties;
End;

{ TListViewReporter }

Procedure TListViewReporter.AssignClass;
Begin
  If (FListView.SelectedData = Nil) Then
    FListView.PositionCursor(0);
  FReportClass := FListView.SelectedData;
End;

Constructor TListViewReporter.Create;
Begin
  Inherited;
  FPipeLine.InitialIndex := 0;
  FPipeLine.OnCheckEOF := Nil;
End;

Function TListViewReporter.GetItemCount : Integer;
Begin
  If (FListView <> Nil) Then
    Result := FListView.Items.Count
  Else
    Result := 0;
End;

Procedure TListViewReporter.GetObjectProperties;
Var
  I : Integer;
  lFieldInfo : TFieldInfo;
  lColumn : TtiListColumn;
Begin
  AssignClass;
  With FListView Do
  Begin
    FPropList.Clear;
    For I := 0 To Pred(ListColumns.Count) Do
    Begin
      lColumn := ListColumns.Items[I];
      lFieldInfo := TFieldInfo.Create;
      lFieldInfo.fiFieldTitle := lColumn.DisplayLabel;
      lFieldInfo.fiFieldName := lColumn.FieldName;
      If lColumn.Derived Then
        lFieldInfo.fiRBDataType := dtString
      Else
        lFieldInfo.fiRBDataType := GetRBDataType(FReportClass, lFieldInfo.fiFieldName);
      lFieldInfo.fiWidth := lColumn.Width;
      FPropList.Add(lFieldInfo);
//      Log('Field %d is %s and is %d wide.', [I, lFieldInfo.fiFieldName, lFieldInfo.fiWidth]);
    End; { Loop }
  End;
End;

Function TListViewReporter.plCheckEOF : Boolean;
Begin
//  Result := (FPipeLine.RecordIndex >= FListView.Items.Count);
  Result := False;
End;

Function TListViewReporter.plGetFieldValue(aFieldName : String) : Variant;
Var
  lListColumn : TtiListColumn;
  lColID : Integer;
Begin
  If (FListView.SelectedIndex <> FPipeLine.RecordIndex) Then
  Begin
    FListView.PositionCursor(FPipeLine.RecordIndex);
  End;
  If (FListView.SelectedData <> Nil) Then
  Begin
    lListColumn := FListView.ListColumns.FindByFieldName(aFieldName);
    If Assigned(lListColumn) Then
      lColID := lListColumn.ID
    Else
    Begin
     // ErrorBox('Error getting column for field ' + aFieldName);
      Result := '';
      Exit;
    End;
    If lListColumn.Derived Then
      Result := FListView.Selected.SubItems.Strings[lColID]
    Else
      Result := GetPropValue(FListView.SelectedData, aFieldName);
  End
  Else
    Result := 'Error';
End;

Procedure TListViewReporter.SetListView(pLV : TtiCustomListView);
Begin
  FListView := pLV;
  FPipeLine.RecordCount := pLV.Items.Count;
  GetObjectProperties;
//  Log('Record count is %d', [FPipeLine.RecordCount]);
End;

{ TVtListViewReporter }

Procedure TVtListViewReporter.AssignClass;
Begin
//    Log('Assigning class...');
  FReportClass := TtiObject(FListView.GetObjectFromNode(FListView.VT.GetFirst));
End;

Constructor TVtListViewReporter.Create;
Begin
  Inherited;
  FPipeLine.InitialIndex := 0;
  FLastIndex := -1;
End;

Function TVtListViewReporter.GetItemCount : Integer;
Begin
  If Assigned(FListView) Then
    Result := FListView.VT.RootNodeCount
  Else
    Result := 0;
End;

Function TVtListViewReporter.GetNodeNumber(pNodeNo : Integer) : PVirtualNode;
Var
  lResult : PVirtualNode;
  I : Integer;
Begin
  lResult := FListView.VT.GetFirst;
  I := FPipeLine.InitialIndex;
  While (lResult <> Nil) And (I <> pNodeNo) Do
  Begin
    lResult := FListView.VT.GetNextSibling(lResult);
    Inc(I);
  End;
  Result := lResult;
End;

Procedure TVtListViewReporter.GetObjectProperties;
Var
  I : Integer;
  lFieldInfo : TFieldInfo;
  lColumn : TtiVTColumn;
Begin
  AssignClass;
  With FListView Do
  Begin
    FPropList.Clear;
    With Header.Columns Do
    Begin
      I := GetFirstVisibleColumn; // Do it this way instead of a For.. loop to
      While (I >= 0) Do // respect any runtime column dragging by the user
      Begin
        lColumn := TtiVTColumn(Items[I]);
        lFieldInfo := TFieldInfo.Create;
        lFieldInfo.fiFieldTitle := lColumn.Text;
        lFieldInfo.fiFieldName := lColumn.FieldName;
        If lColumn.Derived Then
          lFieldInfo.fiRBDataType := dtString
        Else
        Begin
          Case lColumn.DataType Of
            vttkDateTime : lFieldInfo.fiRBDataType := dtDateTime;
            vttkTime : lFieldInfo.fiRBDataType := dtTime;
            vttkDate : lFieldInfo.fiRBDataType := dtDate;
            vttkInt : lFieldInfo.fiRBDataType := dtInteger;
            vttkFloat : lFieldInfo.fiRBDataType := dtExtended;
            vttkString :lFieldInfo.fiRBDataType := dtString;
            vttkCurrency : lFieldInfo.fiRBDataType := dtCurrency;
          End;
        End;
        lFieldInfo.fiWidth := lColumn.Width;
        FPropList.Add(lFieldInfo);
        I := Header.Columns.GetNextVisibleColumn(I);
      End;
    End;
  End;
End;

Function TVtListViewReporter.plCheckEOF : Boolean;
Var
  lIndex : Cardinal;
Begin
  lIndex := FPipeLine.RecordIndex; // Do this to avoid compiler warning about comparing unsigned and signed types
  Result := (lIndex >= (FListView.VT.RootNodeCount));
End;

Function TVtListViewReporter.plGetFieldValue(aFieldName : String) : Variant;
Var
//  lObject : TtiObject;
  lColumn : TtiVTColumn;
  lText : String;
Begin
  Try
    With FListView Do
    Begin
      If Not Assigned(FCurrentObject) Or (FLastIndex <> FPipeLine.RecordIndex) Then
      Begin
        FCurrentObject := TtiObject(FListView.GetObjectFromNode(GetNodeNumber(FPipeLine.RecordIndex)));
        FLastIndex := FPipeLine.RecordIndex;
        //Log('Current index is %d',[FPipeline.RecordIndex]);
      End;
      If Assigned(FCurrentObject) Then
      Begin
        lColumn := Header.Columns.FindByFieldName(aFieldName);
        If Assigned(lColumn) Then
        Begin
          If lColumn.Derived Then
          Begin
            lColumn.OnDeriveColumn(FListView, FCurrentObject, lColumn, lText);
            Result := lText;
          End
          Else
            Result := GetPropValue(FCurrentObject, aFieldName);
        End
        Else
          Log('** Error getting column containing field %s **', [aFieldName]);
      End;
{      Else
        Log('** Error getting object property %s for record number %d **', [aFieldName, FPipeLine.RecordIndex]);}
    End;
  Except
    On E : Exception Do
      Result := 'ERROR - ' + E.Message;
  End;
End;

Procedure TVtListViewReporter.PreviewFormCreate(Sender : TObject);
Begin
//
End;

Procedure TVtListViewReporter.SetListView(pLV : TtiVTListView);
Begin
  FListView := pLV;
  FCurrentObject := Nil;
  FLastIndex := -1;
  FPipeLine.RecordCount := GetItemCount + 1;
  If Assigned(pLV) Then
    GetObjectProperties;
End;

{ TListViewPlusReporter }

Procedure TListViewPlusReporter.AssignClass;
Begin
  If (FListViewPlus.SelectedData = Nil) Then
    FListViewPlus.PositionCursor(0);
  FReportClass := FListViewPlus.SelectedData;
End;

Function TListViewPlusReporter.GetItemCount : Integer;
Begin
  Result := FListViewPlus.Items.Count;
End;

Procedure TListViewPlusReporter.GetObjectProperties;
Var
  I : Integer;
  lType : TppDataType;
Begin
  AssignClass;
  With FListViewPlus Do
  Begin
    For I := 0 To ListColsBeingDisplayed.Count - 1 Do
    Begin
      If ListColsBeingDisplayed.Items[I] <> Nil Then
      Begin
        With (ListColsBeingDisplayed.Items[I] As TtiListColumn) Do
        Begin
          lType := dtString;
          Case DataType Of
        //lvtkDerived,
            lvtkString : lType := dtString;
            lvtkFloat,
              lvtkCurrency : lType := dtCurrency;
            lvtkDateTime : lType := dtDateTime;
            lvtkInt : lType := dtInteger;
          End; { Case }
          AddColumnHeading(DisplayLabel, I, lType);
          FPipeLine.DefineField(FieldName, lType, 25); // 25 is a temporary hack value for now.
          AddReportField(FieldName, I, lType);
        End;
      End;
    End; { Loop }
  End;
End;

Function TListViewPlusReporter.plCheckEOF : Boolean;
Begin
  Result := (FPipeLine.RecordIndex >= FListViewPlus.Items.Count);
End;

Function TListViewPlusReporter.plGetFieldValue(aFieldName : String) : Variant;
Var
  lListColumn : TtiListColumn;
  lColID : Integer;
Begin
  FListViewPlus.PositionCursor(FPipeLine.RecordIndex);
  If (FListViewPlus.SelectedData <> Nil) Then
  Begin
    lListColumn := FListViewPlus.ListColumns.FindByFieldName(aFieldName);
    If Assigned(lListColumn) Then
    Begin
      lColID := lListColumn.ID;
      If lListColumn.Derived Then
        Result := FListViewPlus.Selected.SubItems.Strings[lColID]
      Else
        Result := GetPropValue(FListViewPlus.SelectedData, aFieldName);
    End
    Else
      Log('Unable to find ListColumn for field %s!', [aFieldName]);
  End
  Else
    Result := 'Error';
End;

Procedure TListViewPlusReporter.SetListViewPlus(Const Value : TtiListViewPlus);
Begin
  FListViewPlus := Value;
  ClearReport;
  GetObjectProperties;
End;

End.

