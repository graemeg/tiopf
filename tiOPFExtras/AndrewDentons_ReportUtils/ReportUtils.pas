{******************************************************************************}
{*  Provides automatic report generation with Report Builder for any          *}
{*  TPerObjList. For more information on Report Builder please visit the      *}
{*  Digital Metaphors web-site at http://www.digital-metaphors.com            *}
{*                                                                            *}
{*  Written by Andrew Denton (adenton@q-range.com  a_denton@blueyonder.co.uk )*}
{*  Version History :-                                                        *}
{*  1.0a 8th May 2002 - AD - Initial Version.                                 *}
{*  1.0b 17th May 2002 - AD - Added RemoveField method & a write method for   *}
{*                            SourceList which automatically gets the object  *}
{*                            properties.                                     *}
{*                                                                            *}
{******************************************************************************}

Unit ReportUtils;

Interface

Uses
  SysUtils, TypInfo, Contnrs, tiPtnVisPerObj, Classes, tiListView, ppModule,
  daDataModule, ppDB, ppDBJIT, ppCache, ppClass, ppBands, ppComm, ppRelatv,
  ppProd, ppReport, ppVar, ppCtrls, ppPrnabl, ppTypes, ppViewr;

Const
  fmtNoRTTI = 'No RTTI available for class %s.';

Type
  TGetTextEvent = Procedure(Sender : TObject; Var Text : String) Of Object;

  TFieldInfo = Class
    fiPropInfo : TTypeInfo;
    fiFieldTitle : String;
    fiFieldName : String;
    fiRBDataType : TppDataType;
    fiGroupedField : Boolean;
  Public
    Constructor Create;
  End;

  TCustomReporter = Class
  Private
    FItemCount : Integer;
    FCurrXPos : Integer;
    FReportClass : TPersistent; // The class we are reporting on.
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
    Procedure AssignClass; Virtual; Abstract;
    Procedure GetObjectProperties; Virtual;
    Function GetRBDataType(pObject : TObject; Const pProperty : String) : TppDataType;
    Procedure ClearBand(pBand : TppBand);
    Procedure RemoveColumnHeadings;
    Procedure SetupHeader; Virtual;
    Procedure SetupDetail; Virtual;
    Procedure SetupFooter; Virtual;
    Function plCheckEOF : Boolean; Virtual; Abstract;
    Function plGetFieldValue(aFieldName : String) : Variant; Virtual; Abstract;
    Function GetReportFieldWidth(FieldType : TppDataType) : Integer;
    Procedure AddLine(pBand : TppBand; pPos : Integer); Overload;
    Procedure AddLine(pBand : TppBand; pHPos, pVPos, pWidth : Integer); Overload;
    Procedure AddReportField(Const pFieldName : String; FieldNo : Integer;
      FieldType : TppDataType);
    Procedure AddColumnHeading(Const pHeading : String; FieldNo : Integer;
      FieldType : TppDataType);
    Procedure AddSubTotalField(pGroup : TppGroup; pBand : TppBand; pFieldName : String);
    Procedure AddATotalField(pBand : TppBand; pFieldName : String);
    Function FindReportField(Const pName : String) : TppComponent;
    Procedure GetGroupValue(Sender : TObject; Var pBreakValue : String);
    Procedure DefaultHeaderText(Sender : TObject; Var Text : String); Virtual;
    Procedure DefaultFooterText(Sender : TObject; Var Text : String); Virtual;
    Procedure PreviewFormCreate(Sender : TObject); Virtual;
    Function GetItemCount : Integer; Virtual; Abstract;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure SetupReport;
    Procedure PrintTheReport(Const pPrintDest : String);
    Procedure AddGrouping(Const pGroupField, pTotalField : String; pAddHeaderBand, pAddFooterBand : Boolean); Overload;
    Procedure AddGrouping(Const pGroupField : String; pTotalFields : TStringList; pAddHeaderBand, pAddFooterBand : Boolean); Overload;
    Procedure AddTotalField(Const pTotalField : String);
    Procedure AddTotalFields(pTotalFields : TStringList);
    Procedure SetFieldTitle(Const pFNumber : Integer; Const pTitle : String);
    Procedure RemoveField(Const pFieldName : String);
  Published
    Property Report : TppReport Read FReport Write FReport;
    Property FieldList : TObjectList Read FPropList Write FPropList;
    Property ReportTitle : String Read FReportTitle Write FReportTitle;
    Property RangeTitle : String Read FRangeTitle Write FRangeTitle;
    Property IncludeCaption : Boolean Read FIncludeCaption Write FIncludeCaption;
    Property RemoveGroupItemFromReport : Boolean Read FRemoveGroup Write FRemoveGroup;
    Property FieldSpacing : Integer Read FFieldSpacing Write FFieldSpacing;
    Property CurrentGroupValue : String Read FCurrentGroupValue Write FCurrentGroupValue;
    Property ItemCount : Integer Read GetItemCount Write FItemCount;
    {Events}
    Property OnGetGroupHeaderText : TGetTextEvent Read FOnGetGroupHeaderText Write FOnGetGroupHeaderText;
    Property OnGetGroupFooterText : TGetTextEvent Read FOnGetGroupFooterText Write FOnGetGroupFooterText;
  End;

  TPerObjListReporter = Class(TCustomReporter)
  Private
    FSourceList : TPerObjList;
    Function plCheckEOF : Boolean; Override;
    Procedure AssignClass; Override;
    Procedure SetSourceList(Const Value : TPerObjList);
    Function plGetFieldValue(aFieldName : String) : Variant; Override;
    Function GetItemCount : Integer; Override;
  Published
    Property SourceList : TPerObjList Read FSourceList Write SetSourceList;
  End;

  TListViewReporter = Class(TCustomReporter)
  Private
    FListView : TtiCustomListView;
    Procedure AssignClass; Override;
    Procedure GetObjectProperties; Override;
    Function plCheckEOF : Boolean; Override;
    Function plGetFieldValue(aFieldName : String) : Variant; Override;
    Function GetItemCount : Integer; Override;
    Procedure SetListView(pLV : TtiCustomListView);
  Published
    Property ListView : TtiCustomListView Read FListView Write SetListView;
  End;

  ENoRunTimeTypeInfo = Class(Exception);
  EEmptyPerObjList = Class(Exception);

Procedure ShowMaximised(Const pReport : TppReport);

Implementation

Uses Dialogs, Graphics, Forms;


Procedure ShowMaximised(Const pReport : TppReport);
Begin
  pReport.PreviewForm.WindowState := wsMaximized;
  TppViewer(pReport.PreviewForm.Viewer).ZoomSetting := zs100Percent;
End;

{ TCustomReporter }

Procedure TCustomReporter.AddColumnHeading(Const pHeading : String;
  FieldNo : Integer; FieldType : TppDataType);
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
  lblHeading.spWidth := GetReportFieldWidth(FieldType);
  If FieldType In [dtCurrency, dtInteger] Then
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
  lLine.Pen.Width := 2;
  lLine.ParentWidth := True;
  lLine.spTop := pPos;
End;

Procedure TCustomReporter.AddReportField(Const pFieldName : String;
  FieldNo : Integer; FieldType : TppDataType);
Var
  lblField : TppDBText;
Begin
  lblField := TppDBText.Create(Nil);
  lblField.Name := pFieldName;
  lblField.Band := FReport.DetailBand;
  lblField.spLeft := FCurrXPos;
  lblField.spTop := 2;
  lblField.DataPipeline := FPipeLine;
  lblField.AutoSize := True;
  lblField.DataField := pFieldName;
  lblField.spWidth := GetReportFieldWidth(FieldType);
  If (FieldType In [dtCurrency, dtInteger]) Then
    lblField.Alignment := taRightJustify;
  FCurrXPos := FCurrXPos + lblField.spWidth + FieldSpacing;
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
    If (lTempDataField.DataType = dtCurrency) Then
      lTotalField.DisplayFormat := CurrencyString + '0#.00';
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
  FPipeLine := TppJITPipeLine.Create(Nil);
  FReport.DataPipeline := FPipeLine;
  FReport.DefaultBands := FReport.DefaultBands + [btSummary];
  FReport.CreateDefaultBands;
  FReport.DetailBand.spHeight := 12;
  FReport.HeaderBand.Height := 18.0;
  FReport.SummaryBand.Height := 8.5;
  FReport.PrinterSetup.PaperName := 'A4';
  FReport.OnPreviewFormCreate := PreviewFormCreate;
  FReport.AllowPrintToFile := True;
  FPropList := TObjectList.Create;
  FFieldSpacing := 10; // Default field spacing in report units.
  FReportSetup := False;
  FIncludeCaption := False;
  FRemoveGroup := True;
  FRangeTitle := 'All Items';
  FOnGetGroupHeaderText := DefaultHeaderText; //Nil;
  FOnGetGroupFooterText := DefaultFooterText; //Nil;
  // Hook up the event handlers for the pipeline.
  FPipeLine.OnCheckEOF := plCheckEOF;
  FPipeLine.OnGetFieldValue := plGetFieldValue;
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

Procedure TPerObjListReporter.AssignClass;
Begin
  FReportClass := FSourceList.Items[0];
End;

Function TPerObjListReporter.GetItemCount : Integer;
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
      Begin // Bit of a hack this, but RTTI has no DateTime property.
        If (GetTypeData(PropertyPtr.PropType^)^.FloatType = ftCurr) Then
          Result := dtCurrency
        Else If (Pos('TIME', UpperCase(pProperty)) <> 0) Or (Pos('DATE', UpperCase(pProperty)) <> 0) Then
          Result := dtDateTime
        Else
          Result := dtExtended;
      End;
    tkVariant : Result := dtVariant;
    tkUnknown : ShowMessage(Format(fmtNoRTTI, [pObject.ClassName]));
  End; { Case }
End;

Function TCustomReporter.GetReportFieldWidth(FieldType : TppDataType) : Integer;
Begin
  Result := 50;
  Case FieldType Of
    dtDateTime : Result := 100;
    dtInteger : Result := 50;
    dtString : Result := 120;
    dtCurrency : Result := 60;
  End; { Case }
End;

Function TPerObjListReporter.plCheckEOF : Boolean;
Begin
  Result := (FPipeLine.RecordIndex >= SourceList.Count);
End;

Function TPerObjListReporter.plGetFieldValue(aFieldName : String) : Variant;
Var
  lData : TPerObjAbs;
Begin
  Try
    lData := SourceList.Items[FPipeLine.RecordIndex];
    Result := GetPropValue(lData, aFieldName);
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
Begin
  FCurrXPos := 0;
  For I := 0 To FPropList.Count - 1 Do
  Begin
    With TFieldInfo(FPropList.Items[I]) Do
    Begin
      If (FPipeLine.IndexOfFieldName(fiFieldName) = -1) Then
        FPipeLine.DefineField(fiFieldName, fiRBDataType, 25);
      If Not fiGroupedField Or Not FRemoveGroup Then
      Begin
        AddColumnHeading(fiFieldTitle, I, fiRBDataType);
        AddReportField(fiFieldName, I, fiRBDataType);
      End;
    End;
  End; { Loop }
End;

Procedure TCustomReporter.SetupFooter;
Var
  EOPLabel : TppLabel;
Begin
  AddLine(FReport.SummaryBand, 22);
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
  lRangeTitle,
  lReportTitle : TppLabel;
  TitleX,
  TitleY : Integer;
Begin
  AddLine(FReport.HeaderBand, 2);
  lSysVar := TppSystemVariable.Create(Nil);
  lSysVar.VarType := vtPrintDateTime;
  lSysVar.SpTop := 4;
  lSysVar.spLeft := 2;
  lSysVar.Band := FReport.HeaderBand;
  lSysVar := TppSystemVariable.Create(Nil);
  lSysVar.VarType := vtPageSetDesc;
  lSysVar.Band := FReport.HeaderBand;
  lSysVar.Alignment := taRightJustify;
  lSysVar.spLeft := (FReport.PrinterSetup.PageDef.spPrintableWidth -
    lSysVar.spWidth) - 2;
  lSysVar.spTop := 4;
  AddLine(FReport.HeaderBand, FReport.HeaderBand.spHeight - 2);
  lReportTitle := TppLabel.Create(Nil);
  lReportTitle.Band := FReport.HeaderBand;
  lReportTitle.AutoSize := True;
  lReportTitle.Caption := FReportTitle;
  lReportTitle.Font.Size := 14;
  lReportTitle.Font.Style := [fsBold];
  lReportTitle.Alignment := taCenter;
  TitleX := (FReport.PrinterSetup.PageDef.spPrintableWidth -
    lReportTitle.spWidth) Div 2;
  TitleY := 10;
  lReportTitle.spTop := TitleY;
  lReportTitle.spLeft := TitleX;
  lRangeTitle := TppLabel.Create(Nil);
  lRangeTitle.Band := FReport.HeaderBand;
  lRangeTitle.AutoSize := True;
  lRangeTitle.Caption := FRangeTitle;
  lRangeTitle.Font.Style := [fsBold];
  lRangeTitle.Alignment := taCenter;
  TitleX := (FReport.PrinterSetup.PageDef.spPrintableWidth -
    lRangeTitle.spWidth) Div 2;
  TitleY := 32;
  lRangeTitle.spTop := TitleY;
  lRangeTitle.spLeft := TitleX;
End;

Procedure TCustomReporter.SetupReport;
Begin
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

{ TFieldInfo }

Constructor TFieldInfo.Create;
Begin
  fiGroupedField := False;
End;

{ TPerObjListReporter }

Procedure TPerObjListReporter.SetSourceList(Const Value : TPerObjList);
Begin
  FSourceList := Value;
  GetObjectProperties;
End;

{ TListViewReporter }

Procedure TListViewReporter.AssignClass;
Begin
  If (FListView.SelectedData = Nil) Then
    FListView.PositionCursor(0);
  FReportClass := FListView.SelectedData;
End;

Function TListViewReporter.GetItemCount : Integer;
Begin
  If (FListView <> Nil) Then
    Result := FListView.Items.Count
  Else
    Result := 0;
End;

Procedure TListViewReporter.GetObjectProperties;
{Var
  I, OldPos : Integer;}
Begin
  Inherited;
(*  OldPos := FListView.SelectedIndex;
  AssignClass;
(*  For I := 0 To FListView.ListColumns.Count - 1 Do
  Begin
    With FListView.ListColumns.Items[I] Do
    Begin
      MyType := dtString;
      Case DataType Of
        //lvtkDerived,
        lvtkString : MyType := dtString;
        lvtkFloat,
        lvtkCurrency : MyType := dtCurrency;
        lvtkDateTime : MyType := dtDateTime;
        lvtkInt : MyType := dtInteger;
      End; { Case }
      AddColumnHeading(DisplayLabel, I, MyType);
      plData.DefineField(FieldName, MyType, 25); // 25 is a temporary hack value for now.
      AddReportField(FieldName, I, MyType);
    End;
  End; { Loop }
  *)
End;

Function TListViewReporter.plCheckEOF : Boolean;
Begin
  Result := (FPipeLine.RecordIndex >= FListView.Items.Count);
End;

Function TListViewReporter.plGetFieldValue(aFieldName : String) : Variant;
Var
  lListColumn : TtiListColumn;
  lColID : Integer;
Begin
  FListView.PositionCursor(FPipeLine.RecordIndex);
  If (FListView.SelectedData <> Nil) Then
  Begin
    lListColumn := FListView.ListColumns.FindByFieldName(aFieldName);
    lColID := lListColumn.ID;
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
  GetObjectProperties;
End;

End.
