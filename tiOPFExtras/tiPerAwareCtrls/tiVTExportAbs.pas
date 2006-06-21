Unit tiVTExportAbs;

Interface

Uses Classes, tiVTListView;

Type
  TtiVTExportAbs = Class
  Protected
    FFileName : String;
    FDefaultExt : String;
    FSourceListView : TtiVTListView;
    FOutputValid : Boolean;
  Public
    Function GetFileName : Boolean; Virtual;
    Procedure WriteToOutput(Const pText : String); Virtual; Abstract;
    Procedure CreateOutputFile; Virtual;
    Procedure WriteHeader; Virtual;
    Procedure WriteTitles; Virtual;
    Procedure WriteDetail; Virtual;
    Procedure WriteFooter; Virtual;
    Procedure CloseOutputFile; Virtual;
    Procedure Execute;
    Property SourceListView : TtiVTListView Read FSourceListView Write FSourceListView;
    Property FileName : String Read FFileName Write FFileName;
    Property DefaultExt : String Read FDefaultExt Write FDefaultExt;
  End;

Implementation

Uses tiLog, SysUtils, Dialogs;

Const
  fmtFilter = '%s Files|*.%s|All Files|*.*';

{ TtiVTExportAbs }

Procedure TtiVTExportAbs.CloseOutputFile;
Begin

End;

Procedure TtiVTExportAbs.CreateOutputFile;
Begin

End;

Procedure TtiVTExportAbs.Execute;
Begin
  If GetFileName Then
  Begin
    CreateOutputFile;
    WriteHeader;
    WriteTitles;
    WriteDetail;
    WriteFooter;
    CloseOutputFile;
  End;
End;

Function TtiVTExportAbs.GetFileName : Boolean;
Var
  lSaveDialog : TSaveDialog;
Begin
  Result := False;
  LogFmt('DefaultExt of class %s is %s.', [ClassName, DefaultExt]);
  lSaveDialog := TSaveDialog.Create(Nil);
  Try
    lSaveDialog.DefaultExt := Self.DefaultExt;
    lSaveDialog.FileName := Self.FileName;
    lSaveDialog.Filter := Format(fmtFilter,[FDefaultExt, FDefaultExt]);
    If lSaveDialog.Execute Then
    Begin
      FFileName := lSaveDialog.FileName;
      Result := True;
    End;
  Finally
    lSaveDialog.Free;
  End;
End;

Procedure TtiVTExportAbs.WriteDetail;
Begin

End;

Procedure TtiVTExportAbs.WriteFooter;
Begin

End;

Procedure TtiVTExportAbs.WriteHeader;
Begin

End;

Procedure TtiVTExportAbs.WriteTitles;
Begin

End;

End.

