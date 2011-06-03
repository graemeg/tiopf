{******************************************************************************}
{*  Provides automatic report generation with Fast Report for any             *}
{*  TtiObjectList, TtiListView, TtiListViewPlus, or TtiVTListView.            *}
{*  For more information on Fast Report please visit the Fast Report web-site *}
{*  at http://www.fast-report.com                                             *}
{*                                                                            *}
{*  Written by Andrew Denton (a_denton@blueyonder.co.uk,                      *}
{*                            andrewdenton.delphi@gmail.com                   *}
{*  Version History :-                                                        *}
{*                                                                            *}
{*  1.0a 11th June 2008 - AD - 1st release derived from ReportBuilder Version *}
{*                                                                            *}
{******************************************************************************}

Unit FastReportUtils;

Interface

Uses
  SysUtils, TypInfo, Contnrs, tiObject, Classes, tiListView, tiListViewPlus,
  tiVirtualTrees, tiVTListView, frxClass;

Const
  fmtNoRTTI = 'No RTTI available for class %s.';

Type
  TGetTextEvent = Procedure(Sender : TObject; Var Text : String) Of Object;
  TGetValueEvent = Procedure(Const VarName : String; Var Value : Variant) Of Object;
  TMemoEvent = Procedure(Sender : TObject; aLines : TStrings) Of Object;
  TtiRepFieldType = (rftNotKnown, rftString, rftInteger, rftCurrency, rftDateTime,
    rftTime, rftDate, rftExtended, rftVariant);

  TFieldInfo = Class
    fiPropInfo : TTypeInfo;
    fiFieldTitle : String;
    fiFieldName : String;
    fiPicture : String;
    fiWidth : Integer;
    fiRBDataType : TtiRepFieldType;
    fiGroupedField : Boolean;
  Public
    Constructor Create;
  End;

  TCustomReporter = Class
  Private
    FItemCount : Integer;
    FCurrXPos : Integer;
    FReport : TfrxReport;
    FTitleBand : TfrxBand;
    FHeaderBand : TfrxBand;
    FFooterBand : TfrxBand;
    FPage : TfrxReportPage;
    FDataPage : TfrxDataPage;
    FDataBand : TfrxMasterData;
    FRangeTitle : String;
    FReportTitle : String;
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
    FDefaultFontSize : Integer;
    FDateRange : String;
    FCompanyAddress : String;
    FHaveTotalFields : Boolean;
    FMaxTitleLength : Integer;
    Procedure SetLandscape(Const Value : Boolean);
    Procedure SetRangeTitle(Const Value : String);
  Protected
    FReportClass : TtiObject; // The class we are reporting on.
    FPipeLine : TfrxUserDataSet;
    FPropList : TObjectList;
    Procedure CreateReport;
    Procedure CreatePipeLine;
    Procedure CreatePage;
    Procedure CreateHeader;
    Procedure CreateDetail;
    Procedure CreateFooter;
    Procedure AssignClass; Virtual; Abstract;
    Procedure GetObjectProperties; Virtual;
    Function GetRBDataType(pObject : TObject; Const pProperty : String) : TtiRepFieldType;
    Procedure ClearBand(pBand : TfrxBand);
    Procedure RemoveColumnHeadings;
    Procedure SetupHeader; Virtual;
    Procedure SetupDetail; Virtual;
    Procedure SetupFooter; Virtual;
    Procedure plCheckEOF(Sender : TObject; Var Eof : Boolean); Virtual; Abstract;
    Procedure plGetFieldValue(Const VarName : String; Var Value : Variant); Virtual; Abstract;

    Function GetReportFieldWidth(FieldType : TtiRepFieldType) : Integer; Virtual;
    Procedure AddLabel(pBand : TfrxBand; pXPos, YPos : Integer;
      Const pText : String; pBold : Boolean = False; pCentered : Boolean = False);
    Procedure AddLine(pBand : TfrxBand; pPos : Integer); Overload;
    Procedure AddLine(pBand : TfrxBand; pHPos, pVPos, pWidth : Integer); Overload;
    Procedure AddReportField(Const pFieldName : String; FieldNo : Integer;
      FieldType : TtiRepFieldType; pLastField : Boolean = False; pWidth : Integer = 0);
    Procedure AddColumnHeading(Const pHeading : String; FieldNo : Integer;
      FieldType : TtiRepFieldType; pWidth : Integer = 0);
    Procedure AddSubTotalField(pBand : TfrxBand; pFieldName : String);
    Procedure AddATotalField(pBand : TfrxBand; pFieldName : String);
    Function FindReportField(Const pName : String) : TfrxComponent;
    Procedure GetGroupValue(Sender : TObject; Var pBreakValue : String);
    Procedure DefaultHeaderText(Sender : TObject; Var Text : String); Virtual;
    Procedure DefaultFooterText(Sender : TObject; Var Text : String); Virtual;
    Procedure GetFieldText(Sender : TObject; Var Text : String); Virtual;
    Procedure GetMemoText(Sender : TObject; aLines : TStrings); Virtual;
    Function GetItemCount : Integer; Virtual; Abstract;
    Procedure SetReportTitle(Const Value : String);
    Function PrintableWidth : Extended; Virtual;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure SetupReport;
    Procedure ClearReport; Virtual;
    Procedure PrintTheReport(Const pPrintDest : String);
    Procedure PrintToDevices;
    Procedure AddHeaderLabel(X, Y : Integer; Const pText : String);
    Procedure AddGrouping(Const pGroupField, pTotalField : String; pAddHeaderBand, pAddFooterBand : Boolean); Overload;
    Procedure AddGrouping(Const pGroupField : String; pTotalFields : TStringList; pAddHeaderBand, pAddFooterBand : Boolean); Overload;
    Procedure AddTotalField(Const pTotalField : String);
    Procedure AddTotalFields(pTotalFields : TStringList);
    Procedure SetFieldTitle(Const pFNumber : Integer; Const pTitle : String);
    Procedure RemoveField(Const pFieldName : String);
    Property HeaderBand : TfrxBand Read FHeaderBand;
    Property MaxRangeLen : Integer Read FMaxTitleLength Write FMaxTitleLength;
  Published
    Property Report : TfrxReport Read FReport Write FReport;
    Property LandscapePrint : Boolean Read FLandscape Write SetLandscape;
    Property FieldList : TObjectList Read FPropList Write FPropList;
    Property CompanyName : String Read FCompanyName Write FCompanyName;
    Property CompanyAddress : String Read FCompanyAddress Write FCompanyAddress;
    Property ReportTitle : String Read FReportTitle Write SetReportTitle;
    Property RangeTitle : String Read FRangeTitle Write SetRangeTitle;
    Property DateRange : String Read FDateRange Write FDateRange;
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
    Procedure plCheckEOF(Sender : TObject; Var Eof : Boolean); Override;
    Procedure AssignClass; Override;
    Procedure SetSourceList(Const Value : TtiObjectList);
    Procedure plGetFieldValue(Const VarName : String; Var Value : Variant); Override;
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
    Procedure plCheckEOF(Sender : TObject; Var Eof : Boolean); Override;
    Procedure plGetFieldValue(Const VarName : String; Var Value : Variant); Override;
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
    FCurrentObject : TtiObject;
    FLastIndex : Integer;
   { ColumnsDumped : Boolean;
    Procedure DumpColumns;}
  Protected
    Procedure AssignClass; Override;
    Procedure GetObjectProperties; Override;
    Procedure plCheckEOF(Sender : TObject; Var Eof : Boolean); Override;
    Procedure plGetFieldValue(Const VarName : String; Var Value : Variant); Override;
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
    Procedure plCheckEOF(Sender : TObject; Var Eof : Boolean); Override;
    Procedure plGetFieldValue(Const VarName : String; Var Value : Variant); Override;
    Function GetItemCount : Integer; Override;
  Published
    Property ListViewPlus : TtiListViewPlus Read FListViewPlus Write SetListViewPlus;
  End;

  ENoRunTimeTypeInfo = Class(Exception);
  EEmptyPerObjList = Class(Exception);

Implementation

Uses Dialogs, Graphics, Forms, tiLog, Printers;

{ TCustomReporter }

Procedure TCustomReporter.AddColumnHeading(Const pHeading : String;
  FieldNo : Integer; FieldType : TtiRepFieldType; pWidth : Integer = 0);
Var
  lblHeading : TfrxMemoView;
Begin
  If (FCurrXPos < PrintableWidth) Then
  Begin
    lblHeading := TfrxMemoView.Create(FHeaderBand);
    lblHeading.Name := 'lblHeading' + IntToStr(FieldNo);
    lblHeading.Left := FCurrXPos;
    lblHeading.Font.Size := FDefaultFontSize;
    lblHeading.Top := FHeaderBand.Height - 16;
    lblHeading.Memo.Add(pHeading);
    lblHeading.Font.Style := [fsBold];
    If (pWidth <> 0) Then
      lblHeading.Width := pWidth
    Else
      lblHeading.Width := GetReportFieldWidth(FieldType);
    If FieldType In [rftCurrency, rftInteger, rftExtended] Then
      lblHeading.HAlign := haRight;
    lblHeading.SetBounds(FCurrXPos, FHeaderBand.Height - 16, lblHeading.Width, lblHeading.CalcHeight);
  End;
End;

Procedure TCustomReporter.AddGrouping(Const pGroupField, pTotalField : String;
  pAddHeaderBand, pAddFooterBand : Boolean);
{Var
  lGroupLabel : TfrxMemoView;
  lGroup : TppGroup;
  lGroupHeader : TppGroupHeaderBand;
  lGroupFooter : TppGroupFooterBand;
  I : Integer;}
Begin
{  If Not FReportSetup Then
    SetupReport;
  For I := 0 To FPropList.Count - 1 Do
  Begin
    If (TFieldInfo(FPropList.Items[I]).fiFieldName = pGroupField) Then
    Begin
      TFieldInfo(FPropList.Items[I]).fiGroupedField := True;
      Break;
    End;
  End; { Loop }
 { ClearBand(FReport.DetailBand);
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
    lGroupLabel.Top := 2;
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
    lGroupLabel.Top := 2;
    lGroupLabel.AutoSize := True;
    lGroupLabel.spLeft := 2;
    lGroupLabel.OnGetText := OnGetGroupFooterText;
    AddSubTotalField(lGroup, lGroupFooter, pTotalField);
  End;}
End;

Procedure TCustomReporter.AddGrouping(Const pGroupField : String;
  pTotalFields : TStringList; pAddHeaderBand, pAddFooterBand : Boolean);
{Var
  I : Integer;
  lGroup : TppGroup;
  lGroupHeader : TppGroupHeaderBand;
  lGroupFooter : TppGroupFooterBand;
  lGroupLabel : TppLabel;}
Begin
{  If Not FReportSetup Then
    SetupReport;
  For I := 0 To FPropList.Count - 1 Do
  Begin
    If (TFieldInfo(FPropList.Items[I]).fiFieldName = pGroupField) Then
    Begin
      TFieldInfo(FPropList.Items[I]).fiGroupedField := True;
      Break;
    End;
  End; { Loop }
 { ClearBand(FReport.DetailBand);
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
    lGroupLabel.Top := 2;
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
    lGroupLabel.Top := 2;
    lGroupLabel.AutoSize := True;
    lGroupLabel.spLeft := 2;
    lGroupLabel.OnGetText := OnGetGroupFooterText;
    For I := 0 To pTotalFields.Count - 1 Do
      AddSubTotalField(lGroup, lGroupFooter, pTotalFields.Strings[I]);
  End;}
End;

Procedure TCustomReporter.AddLine(pBand : TfrxBand; pHPos, pVPos, pWidth : Integer);
Var
  lLine : TfrxLineView;
Begin
  lLine := TfrxLineView.Create(pBand);
  lLine.CreateUniqueName;
  lLine.SetBounds(pHPos, pVPos, pWidth, 1);
End;

Procedure TCustomReporter.AddLine(pBand : TfrxBand; pPos : Integer);
Var
  lLine : TfrxLineView;
Begin
  Assert((pBand <> Nil), 'Unassigned band passed to AddLine!');
  lLine := TfrxLineView.Create(pBand);
  lLine.CreateUniqueName;
  lLine.SetBounds(0, pPos, pBand.Width, 1);
  lLine.Align := baWidth;
End;

Procedure TCustomReporter.AddReportField(Const pFieldName : String;
  FieldNo : Integer; FieldType : TtiRepFieldType; pLastField : Boolean = False; pWidth : Integer = 0);
Var
  lblField : TfrxMemoView;
  lWidth : Integer;
  lName : String;
Begin
//  Log('Adding field %s at position %d.', [pFieldName, FCurrXPos]);
  If (FCurrXPos < PrintableWidth) Then
  Begin
    lblField := TfrxMemoView.Create(FDataBand);
    lName := StringReplace(pFieldName, ' ', '_', [rfIgnoreCase, rfReplaceAll]);
    lName := StringReplace(lName, '.', '_', [rfIgnoreCase, rfReplaceAll]);
    lblField.Name := lName;
    lblField.Left := FCurrXPos;
    lblField.Top := 0;
    lblField.Font.Size := FDefaultFontSize;
    lblField.WordWrap := True;
    lblField.DataSet := FPipeLine;
    lblField.DataField := pFieldName;
    lblField.StretchMode := smMaxHeight;
    If (FieldType In [rftCurrency, rftInteger, rftExtended]) Then
    Begin
      lblField.HAlign := haRight;
      lblField.DisplayFormat.Kind := fkNumeric;
      If (FieldType = rftExtended) Then
        lblField.DisplayFormat.FormatStr := '%2.2f';
      If (FieldType = rftInteger) Then
        lblField.DisplayFormat.FormatStr := '%d';
      If (FieldType = rftCurrency) Then
        lblField.DisplayFormat.FormatStr := '%2.2m';
    End
    Else If (FieldType In [rftDate, rftTime, rftDateTime]) Then
    Begin
      case FieldType Of
        rftDateTime : Begin
                        lblField.DisplayFormat.Kind := fkDateTime;
                        lblField.DisplayFormat.FormatStr := 'dd/mm/yyyy hh:mm:ss';
                      End;
        rftDate : begin
                    lblField.DisplayFormat.Kind := fkDateTime;
                    lblField.DisplayFormat.FormatStr := 'dd/mm/yyyy';
                  end;
        rftTime : begin
                    lblField.DisplayFormat.Kind := fkDateTime;
                    lblField.DisplayFormat.FormatStr := 'hh:mm:ss';
                  end;
      End; { Case }
    End;
    If Not pLastField Then
    Begin
      If (pWidth = 0) Then
        lWidth := GetReportFieldWidth(FieldType)
      Else
        lWidth := pWidth;
    End
    Else
    Begin
      If (FieldType = rftString) Then // Only expand string fields as numbers look silly.
        lWidth := Trunc(PrintableWidth) - FCurrXPos
      Else
        lWidth := pWidth;
    End;
    lblField.SetBounds(FCurrXPos, 0, lWidth, FDataBand.Height);
    FCurrXPos := FCurrXPos + Trunc(lblField.Width) + FieldSpacing;
  End;
End;

Procedure TCustomReporter.AddSubTotalField(pBand : TfrxBand; pFieldName : String);
{Var
  lTotalField : TppDBCalc;
  lTotalFieldNo : Integer;
  lTempDataField : TppField;
  lTempField : TppComponent;
  lLineTop,
    lLineLeft,
    lLineWidth : Integer;}
Begin
{  lLineWidth := 0;
  lLineLeft := 0;
  lLineTop := 0;
  lTotalField := TppDBCalc.Create(Nil);
  lTotalField.Band := pBand;
  lTotalField.AutoSize := False;
  lTotalField.Alignment := taRightJustify;
  lTotalField.ResetGroup := pGroup;
  lTotalField.DBCalcType := dcSum;
  lTotalField.Top := 6;
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
  AddATotalField(FReport.SummaryBand, pFieldName);}
End;

Procedure TCustomReporter.AddATotalField(pBand : TfrxBand; pFieldName : String);
Var
  lTempField,
    lTotalField : TfrxMemoView;
Begin
  lTotalField := TfrxMemoView.Create(pBand);
  lTempField := TfrxMemoView(FindReportField(pFieldName));
  If Assigned(lTempField) Then
  Begin
    lTotalField.Left := lTempField.Left;
    lTotalField.Width := lTempField.Width;
    lTotalField.Memo.Add('[Sum(<' + FPipeLine.Name + '."' + pFieldName + '">)]');
    lTotalField.Top := 5;
    lTotalField.Font.Size := FDefaultFontSize;
    lTotalField.Font.Style := [fsBold];
    lTotalField.HAlign := lTempField.HAlign;
    lTotalField.DisplayFormat.Kind := lTempField.DisplayFormat.Kind;
    lTotalField.DisplayFormat.FormatStr := lTempField.DisplayFormat.FormatStr;
    lTotalField.SetBounds(lTempField.Left, 5, lTempField.Width, lTempField.Height);
  End;
End;

Procedure TCustomReporter.AddTotalField(Const pTotalField : String);
Begin
  FHaveTotalFields := True;
  If Not FReportSetup Then
    SetupReport;
  AddATotalField(FFooterBand, pTotalField);
End;

Procedure TCustomReporter.AddTotalFields(pTotalFields : TStringList);
Var
  I : Integer;
Begin
  For I := 0 To pTotalFields.Count - 1 Do
    AddTotalField(pTotalFields.Strings[I]);
End;

Procedure TCustomReporter.ClearBand(pBand : TfrxBand);
Begin
  pBand.Clear;
End;

Constructor TCustomReporter.Create;
Begin
  Inherited;
  FLandScape := False;
  FDefaultFontSize := 8;
  FMaxTitleLength := 255;
  FHaveTotalFields := False;
  CreateReport;
  CreatePipeLine;
  CreatePage;
  CreateHeader;
  CreateDetail;
  CreateFooter;

  FPropList := TObjectList.Create;
  FFieldSpacing := 2; // Default field spacing in report units.
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
  FReport.Clear;
  FReport.Free;
End;

Function TCustomReporter.FindReportField(Const pName : String) : TfrxComponent;
Begin
  Result := FReport.FindObject(pName);
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
//  FPipeLine.InitialIndex := 0;
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

Function TCustomReporter.GetRBDataType(pObject : TObject; Const pProperty : String) : TtiRepFieldType;
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
  Result := rftNotknown;
  { Determine property type and return Report Builder data type accordingly..}
  Case PropertyKind(pObject, pProperty) Of
    tkClass,
      tkString,
      tkLString,
      tkWString,
      tkEnumeration : Result := rftString;
    tkInt64,
      tkInteger : Result := rftInteger;
    tkChar : Result := rftString;
    tkFloat :
      Begin
        If (GetTypeData(PropertyPtr.PropType^)^.FloatType = ftCurr) Then
          Result := rftCurrency
        Else
        Begin
          lDateInfo := TypeInfo(TDateTime);
          lDateTest := GetPropInfo(pObject, pProperty);
          If (lDateInfo = lDateTest^.PropType^) Then
            Result := rftDateTime
          Else
            Result := rftExtended;
        End;
      End;
    tkVariant : Result := rftVariant;
    tkUnknown : Result := rftString;
  End; { Case }
End;

Function TCustomReporter.GetReportFieldWidth(FieldType : TtiRepFieldType) : Integer;
Begin
  Result := 150;
  Case FieldType Of
    rftDateTime : Result := 150;
    rftInteger : Result := 50;
    rftString : Result := 150;
    rftExtended : Result := 60;
    rftCurrency : Result := 60;
  End; { Case }
End;

Procedure TtiObjectListReporter.plCheckEOF(Sender : TObject; Var Eof : Boolean);
Begin
  EOF := False;
End;

Procedure TtiObjectListReporter.plGetFieldValue(Const VarName : String; Var Value : Variant);
Var
  lData : TtiObject;
Begin
  Try
    If VarName = 'RECNO' Then
      Value := FPipeLine.RecNo + 1
    Else
    Begin
      lData := SourceList.Items[FPipeLine.RecNo];
      Value := GetPropValue(lData, VarName);
    End;
  Except
    Value := 'Error.';
  End;
End;

Procedure TCustomReporter.PrintTheReport(Const pPrintDest : String);
Begin
  If Not FReportSetup Then
    SetupReport;
  FReport.PrepareReport;
  FReport.Print;
End;

Procedure TCustomReporter.RemoveColumnHeadings;
Var
  I : Integer;
Begin
  With FHeaderBand Do
  Begin
    For I := ComponentCount - 1 Downto 0 Do
    Begin
      If (Copy(Components[I].Name, 1, 10) = 'lblHeading') Then
        Components[I].Destroy;
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
  lBand : TfrxMemoView;
  lFieldInfo : TFieldInfo;
Begin
  lBand := TfrxMemoView.Create(FDataBand);
  lBand.CreateUniqueName;
  lBand.Height := FDataBand.Height;
  lBand.Width := PrintableWidth;
//  lBand.Name := 'shpDetail';
  lBand.SetBounds(0, 0, PrintableWidth, FDataBand.Height);
  lBand.Highlight.Color := clInfoBk;
  lBand.StretchMode := smMaxHeight;
  lBand.Highlight.Condition := '<Line> Mod 2 = 0';
  FCurrXPos := 0;
  If ShowRecordNumber Then
  Begin
    lFieldInfo := TFieldInfo.Create;
    lFieldInfo.fiFieldTitle := '#';
    lFieldInfo.fiFieldName := 'RECNO';
    lFieldInfo.fiWidth := 50;
    lFieldInfo.fiRBDataType := rftInteger;
    FPropList.Insert(0, lFieldInfo);
  End;
  For I := 0 To FPropList.Count - 1 Do
  Begin
    With TFieldInfo(FPropList.Items[I]) Do
    Begin
      If Not FPipeLine.HasField(fiFieldName) Then
        FPipeLine.Fields.Add(fiFieldName);
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
  lLineNo : Integer;
Begin
  AddLine(FFooterBand, 1);
  lLineNo := 4;
  If FHaveTotalFields Then
  Begin
    AddLabel(FFooterBand, 2, lLineNo, 'Totals', True);
    lLineNo := lLineNo + 20;
    AddLine(FFooterBand, lLineNo);
    lLineNo := lLineNo + 2;
  End;
  If ShowRecordCount Then
  Begin
    AddLabel(FFooterBand, 2, lLineNo, Format('%d Item(s) Reported', [ItemCount]), True);
    lLineNo := lLineNo + 20;
    AddLine(FFooterBand, lLineNo);
    lLineNo := lLineNo + 2;
  End;
  AddLabel(FFooterBand, 1, lLineNo, '- - End Of Report - -', True, True);
End;

Procedure TCustomReporter.SetupHeader;
Var
  lPrintDate,
    lPageNo,
    lCompanyName,
    lCompanyAddr,
    lRangeTitle,
    lDateRange,
    lReportTitle : TfrxMemoView;
  lShape : TfrxShapeView;
Begin
  lCompanyName := TfrxMemoView.Create(FTitleBand);
  lCompanyName.Name := 'lblCompanyName';
  lCompanyName.Font.Size := 14;
  lCompanyName.Font.Style := [fsBold];
  lCompanyName.Font.Color := clSkyBlue;
  lCompanyName.Memo.Add(FCompanyName);
  lCompanyName.SetBounds(0, 0, lCompanyName.CalcWidth, lCompanyName.CalcHeight);

  lReportTitle := TfrxMemoView.Create(FTitleBand);
  lReportTitle.Name := 'lblReportTitle';
  lReportTitle.SetBounds(0, 22, 300, 25);
  lReportTitle.Memo.Add(FReportTitle);
  lReportTitle.Font.Size := 12;
  lReportTitle.Font.Style := [fsBold];
  lReportTitle.Font.Color := clGray;

  lRangeTitle := TfrxMemoView.Create(FTitleBand);
  lRangeTitle.Name := 'lblRangeTitle';
  lRangeTitle.Memo.Add(FRangeTitle);
  lRangeTitle.StretchMode := smActualHeight;
  lRangeTitle.SetBounds(0, 40, 750, lRangeTitle.CalcHeight);
  lRangeTitle.Font.Style := [fsBold];
//  lRangeTitle.BeforePrint :=

  lCompanyAddr := TfrxMemoView.Create(FTitleBand);
  lCompanyAddr.Name := 'lblCompanyAddr';
  lCompanyAddr.Memo.Add(FCompanyAddress);
  lCompanyAddr.SetBounds(PrintableWidth - 300, 0, 300, lCompanyAddr.CalcHeight);
  lCompanyAddr.HAlign := haRight;
  lCompanyAddr.Font.Style := [fsBold];
  lCompanyAddr.Font.Color := clGray;

  lDateRange := TfrxMemoView.Create(FTitleBand);
  lDateRange.Name := 'lblDateRange';
  lDateRange.Memo.Add(FDateRange);
  lDateRange.SetBounds(0, 75, 600, lDateRange.CalcHeight);
  lDateRange.Font.Style := [fsBold];
  lDateRange.ShiftMode := smWhenOverlapped;

  lPrintDate := TfrxMemoView.Create(FHeaderBand);
  lPrintDate.Name := 'lblPrintDate';
  lPrintDate.Memo.Add('[<date> + <time>]');
  lPrintDate.DisplayFormat.FormatStr := 'mmmm dd, yyyy hh:mm:ss';
  lPrintDate.DisplayFormat.Kind := fkDateTime;
  lPrintDate.Font.Style := [fsBold];
  lPrintDate.SetBounds(0, FHeaderBand.Height - 33, 300, lPrintDate.CalcHeight);
  lPrintDate.ShiftMode := smWhenOverlapped;

  lPageNo := TfrxMemoView.Create(FHeaderBand);
  lPageNo.Name := 'lblPageNo';
  lPageNo.SetBounds(FPage.Width - lPageNo.Width, FHeaderBand.Height - 33, 100, 17);
  lPageNo.HAlign := haRight;
  lPageNo.Align := baRight;
  lPageNo.Memo.Add('Page [Page#] of [TotalPages#]');
  lPageNo.Font.Style := [fsBold];

  lShape := TfrxShapeView.Create(FHeaderBand);
  lShape.Name := 'shpHeader';
  lShape.Shape := skRectangle;
  lShape.Width := PrintableWidth;
  lShape.Height := 17;
  lShape.Top := FHeaderBand.Height - 18;
  lShape.SetBounds(0, FHeaderBand.Height - 18, PrintableWidth, 17);
End;

Procedure TCustomReporter.SetupReport;
Begin
  If FReportSetup Then
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

Procedure TCustomReporter.ClearReport;
Begin
  FReport.Clear;
  FCurrXPos := 0;
  FHaveTotalFields := False;
  CreatePage;
  CreateHeader;
  CreateDetail;
  CreateFooter;
End;

Procedure TCustomReporter.PrintToDevices;
Begin
  If (GetItemCount > 0) Then
  Begin
    If Not FReportSetup Then
      SetupReport;
    FReport.PrepareReport;
  End;
{  Else
    ErrorBox('Nothing to print!');}
End;

Procedure TCustomReporter.SetLandscape(Const Value : Boolean);
Begin
  FLandscape := Value;
  If FLandscape Then
    FPage.Orientation := poLandscape
  Else
    FPage.Orientation := poPortrait;
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

Procedure TCustomReporter.SetReportTitle(Const Value : String);
Begin
  FReportTitle := Value;
End;

Procedure TCustomReporter.AddHeaderLabel(X, Y : Integer; Const pText : String);
Var
  lblHeading : TfrxMemoView;
Begin
  lblHeading := TfrxMemoView.Create(FHeaderBand);
  lblHeading.CreateUniqueName;
//  lblHeading.Name := 'lblHeading' + IntToStr(FCurrLabelNo);
  lblHeading.Left := X;
  lblHeading.Top := Y;
  lblHeading.Font.Size := FDefaultFontSize;
  lblHeading.SetBounds(X, Y, 200, 16);
//  lblHeading.AutoSize := True;
  lblHeading.Memo.Add(pText);
  Inc(FCurrLabelNo);
  lblHeading.Font.Style := [fsBold];
End;

Procedure TCustomReporter.AddLabel(pBand : TfrxBand; pXPos, YPos : Integer;
  Const pText : String; pBold : Boolean = False; pCentered : Boolean = False);
Var
  lLabel : TfrxMemoView;
  lXPos : Extended;
Begin
  lLabel := TfrxMemoView.Create(pBand);
  lLabel.CreateUniqueName;
  lLabel.Lines.Add(pText);
  lLabel.Font.Size := FDefaultFontSize;
  If pBold Then
    lLabel.Font.Style := [fsBold];
  If Not pCentered Then
    lXPos := pXPos
  Else
    lXPos := (PrintableWidth - lLabel.CalcWidth) / 2;
  lLabel.Left := lXPos;
  lLabel.SetBounds(lXPos, YPos, lLabel.CalcWidth, 18);
End;

Procedure TCustomReporter.CreateDetail;
Begin
  FDataBand := TfrxMasterData.Create(FPage);
  FDataBand.CreateUniqueName;
  FDataBand.DataSet := FPipeline;
  FDataBand.Top := FHeaderBand.Height + 1;
  FDataBand.Height := 18;
  FDataBand.Width := FPage.Width;
  FDataBand.Stretched := True;
End;

Procedure TCustomReporter.CreateFooter;
Begin
  FFooterBand := TfrxReportSummary.Create(FPage);
  FFooterBand.CreateUniqueName;
  FFooterBand.Top := FHeaderBand.Height + FDataBand.Height + 1;
  FFooterBand.Height := 100;
  FFooterBand.Width := FPage.Width;
End;

Procedure TCustomReporter.CreateHeader;
Begin
  FTitleBand := TfrxReportTitle.Create(FPage);
  FTitleBand.CreateUniqueName;
  FTitleBand.Top := 0;
  FTitleBand.Height := 100;
  FTitleBand.Width := FPage.Width;
//  FTitleBand.Stretched := True;
  FTitleBand.SetBounds(0, 0, FPage.Width, 100);

  FHeaderBand := TfrxPageHeader.Create(FPage);
  FHeaderBand.CreateUniqueName;
  FHeaderBand.Top := FTitleBand.Height + 1;
  FHeaderBand.Height := 25;
  FHeaderBand.Width := FPage.Width;
  FHeaderBand.SetBounds(0, 0, FPage.Width, 25);
//  FHeaderBand.Stretched := True; // <-- setting this to true causes a stack overflow for some reason.
End;

Procedure TCustomReporter.CreatePage;
Begin
  FReport.DataSets.Add(FPipeLine);
  FDataPage := TfrxDataPage.Create(FReport);
  FPage := TfrxReportPage.Create(FReport);
  FPage.CreateUniqueName;
  FPage.SetDefaults;
  If FLandscape Then
    FPage.Orientation := poLandscape
  Else
    FPage.Orientation := poPortrait;
End;

Procedure TCustomReporter.CreatePipeLine;
Begin
  FPipeLine := TfrxUserDataSet.Create(Nil);
  FPipeLine.RangeEnd := reCount;
  FPipeLine.RangeEndCount := GetItemCount;
  FPipeLine.Name := 'plReporter';
  FPipeLine.OnCheckEOF := plCheckEOF;
  FPipeLine.OnGetValue := plGetFieldValue;
End;

Procedure TCustomReporter.CreateReport;
Begin
  FReport := TfrxReport.Create(Nil);
  FReport.Name := 'rptAutoReport';
End;

Function TCustomReporter.PrintableWidth : Extended;
Var
  lMargins : Extended;
Begin
  lMargins := (FPage.LeftMargin * fr01cm) + (FPage.RightMargin * fr01cm);
  Result := (FPage.PaperWidth * fr01cm) - lMargins;
End;

Procedure TCustomReporter.SetRangeTitle(Const Value : String);
Begin
  If (Length(Value) <= MaxRangeLen) Then
    FRangeTitle := Value
  Else
    FRangeTitle := '** Range Too Big To Be Printed **';
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
  FPipeLine.RangeEndCount := GetItemCount;
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
//  FPipeLine.InitialIndex := 0;
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
        lFieldInfo.fiRBDataType := rftString
      Else
        lFieldInfo.fiRBDataType := GetRBDataType(FReportClass, lFieldInfo.fiFieldName);
      lFieldInfo.fiWidth := lColumn.Width;
      FPropList.Add(lFieldInfo);
//      Log('Field %d is %s and is %d wide.', [I, lFieldInfo.fiFieldName, lFieldInfo.fiWidth]);
    End; { Loop }
  End;
End;

Procedure TListViewReporter.plCheckEOF(Sender : TObject; Var Eof : Boolean);
Begin
  EOF := False;
End;

Procedure TListViewReporter.plGetFieldValue(Const VarName : String; Var Value : Variant);
Var
  lListColumn : TtiListColumn;
  lColID : Integer;
Begin
  If (FListView.SelectedIndex <> FPipeLine.RecNo) Then
  Begin
    FListView.PositionCursor(FPipeLine.RecNo);
  End;
  If (FListView.SelectedData <> Nil) Then
  Begin
    lListColumn := FListView.ListColumns.FindByFieldName(VarName);
    If Assigned(lListColumn) Then
      lColID := lListColumn.ID
    Else
    Begin
     // ErrorBox('Error getting column for field ' + VarName);
      Value := '';
      Exit;
    End;
    If lListColumn.Derived Then
      Value := FListView.Selected.SubItems.Strings[lColID]
    Else
      Value := GetPropValue(FListView.SelectedData, VarName);
  End
  Else
    Value := 'Error';
End;

Procedure TListViewReporter.SetListView(pLV : TtiCustomListView);
Begin
  FListView := pLV;
  FPipeLine.RangeEndCount := pLV.Items.Count;
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
  FLastIndex := -1;
//  ColumnsDumped := False;
End;


(*Procedure TVtListViewReporter.DumpColumns;
Var
  I : Integer;
Begin
  For I := 0 To Pred(FListView.Header.Columns.Count) Do
  Begin
    Log('Column %d is %s', [I, FListView.Header.Columns.Items[I].FieldName]);
  End; { Loop }
  ColumnsDumped := True;
End;
  *)

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
  I := 0;
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
  FPropList.Clear;
  FPipeLine.Fields.Clear;
  With FListView.Header.Columns Do
  Begin
    I := GetFirstVisibleColumn; // Do it this way instead of a For.. loop to
    While (I >= 0) Do // respect any runtime column dragging by the user
    Begin
      lColumn := TtiVTColumn(Items[I]);
      lFieldInfo := TFieldInfo.Create;
      lFieldInfo.fiFieldTitle := lColumn.Text;
      lFieldInfo.fiFieldName := lColumn.FieldName;
      If lColumn.Derived Then
        lFieldInfo.fiRBDataType := rftString
      Else
      Begin
        Case lColumn.DataType Of
          vttkTime : lFieldInfo.fiRBDataType := rftTime;
          vttkDate : lFieldInfo.fiRBDataType := rftDate;
          vttkDateTime : lFieldInfo.fiRBDataType := rftDateTime;
          vttkInt : lFieldInfo.fiRBDataType := rftInteger;
          vttkFloat : lFieldInfo.fiRBDataType := rftExtended;
          vttkString : lFieldInfo.fiRBDataType := rftString;
          vttkCurrency : lFieldInfo.fiRBDataType := rftCurrency;
        End;
      End;
      lFieldInfo.fiWidth := lColumn.Width;
      FPropList.Add(lFieldInfo);
      FPipeLine.Fields.Add(lFieldInfo.fiFieldName);
      I := GetNextVisibleColumn(I);
    End;
  End;
End;

Procedure TVtListViewReporter.plCheckEOF(Sender : TObject; Var Eof : Boolean);
Var
  lIndex : Cardinal;
Begin
  lIndex := FPipeLine.RecNo; // Do this to avoid compiler warning about comparing unsigned and signed types
  EOF := (lIndex >= (FListView.VT.RootNodeCount));
End;

Procedure TvtListViewReporter.plGetFieldValue(Const VarName : String; Var Value : Variant);
Var
  lColumn : TtiVTColumn;
  lText : String;
Begin
  Try
    With FListView Do
    Begin
      If Not Assigned(FCurrentObject) Or (FLastIndex <> FPipeLine.RecNo) Then
      Begin
        FCurrentObject := TtiObject(FListView.GetObjectFromNode(GetNodeNumber(FPipeLine.RecNo)));
        FLastIndex := FPipeLine.RecNo;
        //Log('Current index is %d',[FPipeline.RecNo]);
      End;
      If Assigned(FCurrentObject) Then
      Begin
        lColumn := Header.Columns.FindByFieldName(VarName);
        If Assigned(lColumn) Then
        Begin
          If lColumn.Derived Then
          Begin
            lColumn.OnDeriveColumn(FListView, FCurrentObject, lColumn, lText);
            Value := lText;
          End
          Else
            Value := GetPropValue(FCurrentObject, VarName);
        End
        Else
        Begin
          Value := 'Error';
         // If Not ColumnsDumped Then
        //  Begin
        //    Log('** Error getting column containing field %s **', [VarName]);
            //DumpColumns;
        //  End;
        End;
      End;
{      Else
        Log('** Error getting object property %s for record number %d **', [VarName, FPipeLine.RecNo]);}
    End;
  Except
    On E : Exception Do
      Value := 'ERROR - ' + E.Message;
  End;
End;

Procedure TVtListViewReporter.SetListView(pLV : TtiVTListView);
Begin
  FListView := pLV;
  FCurrentObject := Nil;
  FLastIndex := -1;
  FPipeLine.RangeEndCount := GetItemCount + 1;
  If Assigned(pLV) Then
  Begin
    GetObjectProperties;
    {Log('Setting listview. These are the columns..');
    DumpColumns;
    ColumnsDumped := False;}
  End
  Else
  Begin
    FPipeline.Fields.Clear;
    FPropList.Clear;
  End;
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
  lType : TtiRepFieldType;
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
          lType := rftString;
          Case DataType Of
        //lvtkDerived,
            lvtkString : lType := rftString;
            lvtkFloat,
              lvtkCurrency : lType := rftCurrency;
            lvtkDateTime : lType := rftDateTime;
            lvtkInt : lType := rftInteger;
          End; { Case }
          AddColumnHeading(DisplayLabel, I, lType);
          FPipeLine.Fields.Add(FieldName);
//          FPipeLine.DefineField(FieldName, lType, 25); // 25 is a temporary hack value for now.
          AddReportField(FieldName, I, lType);
        End;
      End;
    End; { Loop }
  End;
End;

Procedure TListViewPlusReporter.plCheckEOF(Sender : TObject; Var Eof : Boolean);
Begin
  EOF := (FPipeLine.RecNo >= FListViewPlus.Items.Count);
End;

Procedure TListViewPlusReporter.plGetFieldValue(Const VarName : String; Var Value : Variant);
Var
  lListColumn : TtiListColumn;
  lColID : Integer;
Begin
  FListViewPlus.PositionCursor(FPipeLine.RecNo);
  If (FListViewPlus.SelectedData <> Nil) Then
  Begin
    lListColumn := FListViewPlus.ListColumns.FindByFieldName(VarName);
    If Assigned(lListColumn) Then
    Begin
      lColID := lListColumn.ID;
      If lListColumn.Derived Then
        Value := FListViewPlus.Selected.SubItems.Strings[lColID]
      Else
        Value := GetPropValue(FListViewPlus.SelectedData, VarName);
    End
    Else
      Log('Unable to find ListColumn for field %s!', [VarName]);
  End
  Else
    Value := 'Error';
End;

Procedure TListViewPlusReporter.SetListViewPlus(Const Value : TtiListViewPlus);
Begin
  FListViewPlus := Value;
  ClearReport;
  GetObjectProperties;
End;

End.

