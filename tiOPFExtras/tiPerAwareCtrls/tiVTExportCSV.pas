Unit tiVTExportCSV;

Interface

Uses tiVTExportFactory, tiVTExportAbs;

Type
  TtiVTExportCSV = Class(TtiVTExportAbs)
  Private
    FOutputStr : String;
    FCSVFile : Text;
  Public
    Function GetFileName : Boolean; Override;
    Procedure WriteToOutput(Const pText : String); Override;
    Procedure CreateOutputFile; Override;
    Procedure WriteDetail; Override;
    Procedure CloseOutputFile; Override;
  End;

Resourcestring
  CSVDesc = 'CSV';

Implementation

Uses SysUtils, Classes, Dialogs, VirtualTrees;

{ TtiVTExportCSV }

Procedure TtiVTExportCSV.CloseOutputFile;
Begin
  CloseFile(FCSVFile);
End;

Procedure TtiVTExportCSV.CreateOutputFile;
Begin
  Try
    AssignFile(FCSVFile, FFileName);
    Rewrite(FCSVFile);
    FOutputValid := True;
  Except
    On E : Exception Do
    Begin
      ShowMessage(Format('Error creating output file %s (%s).', [FFileName, E.Message]));
      FOutputValid := False;
    End;
  End;
End;

Function TtiVTExportCSV.GetFileName : Boolean;
Begin
  DefaultExt := 'CSV';
  Result := Inherited GetFileName;
End;

Procedure TtiVTExportCSV.WriteDetail;
Var
  lNode : PVirtualNode;
Begin
  With FSourceListView Do
  Begin
    lNode := GetFirst;
    While (lNode <> Nil) Do
    Begin
      FOutputStr := GetRowAsString(lNode, '', ',');
      Delete(FOutputStr,Length(FOutputStr),1); // Remove the last comma as it might confuse some other programs.
      WriteToOutput(FOutputStr);
      lNode := lNode.NextSibling;
    End;
  End;
End;

Procedure TtiVTExportCSV.WriteToOutput(Const pText : String);
Begin
  WriteLn(FCSVFile, pText);
End;

Initialization
  FactoryExport.RegisterExport(CSVDesc, TtiVTExportCSV);
End.

