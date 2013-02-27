Unit tiVTEditors;

Interface

Uses Classes {$IFDEF VER130} ,  DsgnIntf; {$ELSE} , DesignIntf, DesignEditors;{$ENDIF VER130}

Type
  TCEAbout = Class(TDefaultEditor)
    Function GetVerbCount : Integer; Override;
    Function GetVerb(Index : Integer) : String; Override;
    Procedure ExecuteVerb(Index : Integer); Override;
  End;

  TCEVTColumnsEditor = Class(TCEAbout)
  Protected
  Public
    Procedure EditColumns;
    Function GetVerbCount : Integer; Override;
    Function GetVerb(Index : Integer) : String; Override;
    Procedure ExecuteVerb(Index : Integer); Override;
  End;

Implementation

Uses Dialogs, ColnEdit, tiVTListView;

{ TCEAbout }

Procedure TCEAbout.ExecuteVerb(Index : Integer);
Begin
  If (Index = 0) Then
    MessageDlg('TtiVTListView Version 1.0a'#13 + 'Written by Andrew Denton'#13 +
      'Part of tiOPF (TechInsite Object Persistence Framework)'#13 +
      'http://www.techinsite.com.au/tiOPF', mtInformation, [mbOK], 0);
End;

Function TCEAbout.GetVerb(Index : Integer) : String;
Begin
  If (Index = 0) Then
    Result := '&About this component....';
End;

Function TCEAbout.GetVerbCount : Integer;
Begin
  Result := 1;
End;

{ TCEVTColumnsEditor }

Procedure TCEVTColumnsEditor.EditColumns;
Begin
  ShowCollectionEditor(Designer, Component,TtiVTListView(Component).Header.Columns, 'Columns');
End;

Procedure TCEVTColumnsEditor.ExecuteVerb(Index : Integer);
Begin
  If Index < Inherited GetVerbCount Then
    Inherited ExecuteVerb(Index)
  Else If (Index = Inherited GetVerbCount) Then
    EditColumns;
End;

Function TCEVTColumnsEditor.GetVerb(Index : Integer) : String;
Begin
  If Index < Inherited GetVerbCount Then
    Result := Inherited GetVerb(Index)
  Else If (Index = Inherited GetVerbCount) Then
    Result := '&Columns Editor...';
End;

Function TCEVTColumnsEditor.GetVerbCount : Integer;
Begin
  Result := Inherited GetVerbCount + 1;
End;

End.

