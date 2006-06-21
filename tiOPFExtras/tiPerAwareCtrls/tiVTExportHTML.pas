Unit tiVTExportHTML;

Interface

Uses tiVTExportFactory, tiVTExportAbs;

Type
  TtiVTExportHTML = Class(TtiVTExportAbs)
  Private
    FOutputStr : String;
    FHTMLFile : Text;
  Public
    Function GetFileName : Boolean; Override;
    Procedure WriteToOutput(Const pText : String); Override;
    Procedure CreateOutputFile; Override;
    Procedure WriteHeader; Override;
    Procedure WriteTitles; Override;
    Procedure WriteDetail; Override;
    Procedure WriteFooter; Override;
    Procedure CloseOutputFile; Override;
  End;

Resourcestring
  HTMLDesc = 'HTML';

Implementation

Uses SysUtils, Classes, Dialogs, VirtualTrees;

{ TtiVTExportHTML }

Procedure TtiVTExportHTML.CloseOutputFile;
Begin
  CloseFile(FHTMLFile);
End;

Procedure TtiVTExportHTML.CreateOutputFile;
Begin
  Try
    AssignFile(FHTMLFile, FFileName);
    Rewrite(FHTMLFile);
    FOutputValid := True;
  Except
    On E : Exception Do
    Begin
      ShowMessage(Format('Error creating output file %s (%s).', [FFileName, E.Message]));
      FOutputValid := False;
    End;
  End;
End;

Function TtiVTExportHTML.GetFileName : Boolean;
Begin
  FDefaultExt := 'HTM';
  Result := Inherited GetFileName;
End;

Procedure TtiVTExportHTML.WriteDetail;
Var
  lNode : PVirtualNode;
Begin
  With FSourceListView Do
  Begin
    lNode := GetFirst;
    While (lNode <> Nil) Do
    Begin
      FOutputStr := '<TR>' + GetRowAsString(lNode, '<TD>', '</TD>') + '</TR>';
      WriteToOutput(FOutputStr);
      lNode := lNode.NextSibling;
    End;
  End;
  WriteToOutput('</TABLE>');
End;

Procedure TtiVTExportHTML.WriteFooter;
Begin
  WriteToOutput('</TABLE>');
  WriteToOutput('</BODY>');
  WriteToOutput('</HTML>');
End;

Procedure TtiVTExportHTML.WriteHeader;
Begin
  WriteToOutput('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">');
  WriteToOutput('<html>');
  WriteToOutput('  <head>');
  WriteToOutput('    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">');
  WriteToOutput('  </head>');
  WriteToOutput('<body>');
End;

Procedure TtiVTExportHTML.WriteTitles;
Var
  lColumn : TColumnIndex;
  lTempStr : String;
Begin
  WriteToOutput('<TABLE>');
  WriteToOutput('<table BORDER=2 ALIGN="CENTER">');
  WriteToOutput('<COL ALIGN="LEFT" SPAN=' + IntToStr(FSourceListView.VisibleColumnCount) + '>');
  With FSourceListView.Header.Columns Do
  Begin
    lTempStr := '';
    lColumn := GetFirstVisibleColumn;
    While (lColumn > InvalidColumn) Do
    Begin
      lTempStr := lTempStr + '<TD>' + Items[lColumn].Text + '</TD>';
      lColumn := GetNextVisibleColumn(lColumn);
    End;
  End;
  WriteToOutput('<TR><B>' + lTempStr + '</B></TR>');
End;

Procedure TtiVTExportHTML.WriteToOutput(Const pText : String);
Begin
  If FOutputValid Then
    WriteLn(FHTMLFile, pText);
End;

Initialization
  FactoryExport.RegisterExport(HTMLDesc, TtiVTExportHTML);
End.

