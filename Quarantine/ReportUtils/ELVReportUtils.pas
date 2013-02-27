Unit ELVReportUtils;

Interface

Uses tiObject, frxClass, MPCommonUtilities, EasyListView, FastReportUtils, tiELVMediator;

Type
  TEasyListViewReporter = Class(TCustomReporter)
  Private
    FListView : TEasyListView;
{    FCurrentObject : TtiObject;
    FLastIndex : Integer;             }
    FMediator : TtiEasyListViewMediatorView;
    procedure SetMediator(const Value: TtiEasyListViewMediatorView);
  Protected
    Procedure AssignClass; Override;
    Procedure GetObjectProperties; Override;
    Procedure plCheckEOF(Sender : TObject; Var Eof : Boolean); Override;
    Procedure plGetFieldValue(Const VarName : String; Var Value : Variant); Override;
    Function GetItemCount : Integer; Override;
    Procedure SetListView(pLV : TEasyListView);
  Published
    Property ListView : TEasyListView Read FListView Write SetListView;
    Property Mediator : TtiEasyListViewMediatorView Read FMediator Write SetMediator;
  End;


Implementation

Uses tiRTTI, tiLog;

{ TEasyListViewReporter }

Procedure TEasyListViewReporter.AssignClass;
Begin
 // Assert(Mediator <> Nil, 'Mediator has not been assigned!');
//  Assert(FListView <> Nil, 'EasyListView has not been assigned!');
  FReportClass := TtiEasyItemMediator(FListView.Items[0].Data).Model;
End;

Function TEasyListViewReporter.GetItemCount : Integer;
Begin
  If Assigned(FListView) Then
    Result := FListView.Items.Count
  Else
    Result := 0;
End;

Procedure TEasyListViewReporter.GetObjectProperties;
Var
  lColumn : TEasyColumn;
  lFieldInfo : TFieldInfo;
  lType : TtiTypeKind;
Begin
  AssignClass;
  FPipeLine.Fields.Clear;
  FPropList.Clear;
  With FListView.Header Do
  Begin
    lColumn := FirstColumnByPosition;
    While (lColumn <> Nil) Do
    Begin
      lFieldInfo := TFieldInfo.Create;
      lFieldInfo.fiFieldTitle := lColumn.Caption;
      lFieldInfo.fiFieldName := Mediator.PropNameFromCaption(lColumn.Caption);
      lType := tiGetSimplePropType(FReportClass, lFieldInfo.fiFieldName);
      Case lType Of
        tiTKInteger : lFieldInfo.fiRBDataType := rftInteger;
        tiTKFloat : lFieldInfo.fiRBDataType := rftExtended;
        tiTKDateTime : lFieldInfo.fiRBDataType := rftDateTime;
        tiTKString,
        tiTKBoolean : lFieldInfo.fiRBDataType := rftString;
      End; { Case }
      lFieldInfo.fiWidth := lColumn.Width;
      FPropList.Add(lFieldInfo);
      FPipeLine.Fields.Add(lFieldInfo.fiFieldName);
      lColumn := NextColumnByPosition(lColumn);
    End;
  End;
End;

Procedure TEasyListViewReporter.plCheckEOF(Sender : TObject; Var Eof : Boolean);
Begin
  EOF := (FPipeLine.RecNo >= FListView.Items.Count);
End;

Procedure TEasyListViewReporter.plGetFieldValue(Const VarName : String; Var Value : Variant);
Var
  I,
  lColIndex : Integer;
  lCaption : String;
Begin
  lColIndex := -1;
  lCaption := Mediator.CaptionFromPropName(VarName);
  For I := 0 To Pred(FListView.Header.Columns.Count) Do
  Begin
    If (FListView.Header.Columns.Columns[I].Caption = lCaption) Then
    Begin
      lColIndex := I;
      Break;
    End;
  End; { Loop }
  If (lColIndex > -1) Then
    Value := FListView.Items[FPipeLine.RecNo].Captions[lColIndex]
  Else
    Value := 'Ooops!';
End;

Procedure TEasyListViewReporter.SetListView(pLV : TEasyListView);
Begin
  FListView := pLV;
  FPipeLine.RangeEndCount := GetItemCount + 1;
  If Assigned(pLV) And Assigned(Mediator) Then
    GetObjectProperties;
End;



procedure TEasyListViewReporter.SetMediator(const Value: TtiEasyListViewMediatorView);
begin
  FMediator := Value;
  If Assigned(FListView) Then
    GetObjectProperties;
end;

End.

