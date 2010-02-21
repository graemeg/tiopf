
{
  Want colour syntax hilighting?
  Then download SynEdit from http://synedit.sourceforge.net/
  and add the SYNEDIT compiler directive.
}



unit FSQLEditor;

{$I tiDefines.inc}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls ,tiSQLMgr_BOM, ActnList, Menus, ComCtrls, ExtCtrls
  {$IFDEF SYNEDIT}
    ,SynEdit
    ,SynHighlighterSQL
  {$ENDIF}
  {$IFDEF FPC}
  , LResources
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ;

type
  TFormSQLEditor = class(TForm)
    pmSQL: TPopupMenu;
    mnuRunQuery: TMenuItem;
    N1: TMenuItem;
    mnuCopytoclipboard: TMenuItem;
    alMain: TActionList;
    aCopyToClipAsQuoted: TAction;
    aRunQuery: TAction;
    aDefaultParams: TAction;
    aFindInSQL: TAction;
    N2: TMenuItem;
    FindinSQL1: TMenuItem;
    aFindInSQLAgain: TAction;
    FindinSQLagain1: TMenuItem;
    aCopyToClip: TAction;
    Copytoclipboard1: TMenuItem;
    aPasteAsQuotedString: TAction;
    Pasteasquotedstringfromclipboard1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aCopyToClipAsQuotedExecute(Sender: TObject);
    procedure aRunQueryExecute(Sender: TObject);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure aFindInSQLExecute(Sender: TObject);
    procedure aFindInSQLAgainExecute(Sender: TObject);
    procedure aCopyToClipExecute(Sender: TObject);
    procedure aPasteAsQuotedStringExecute(Sender: TObject);
  private
    FData: TSQLMgrQuery;
    FReadOnly: boolean;
//    FFormPickDatabaseObject : TFormPickDatabaseObject ;
    FbPickDatabaseObject : boolean ;
    FDirty: boolean;

    {$IFDEF SYNEDIT}
      FMemoSQL  : TSynEdit ;
      FWMSync   : TSynSQLSyn ;
      FSearchText : string ;
    {$ELSE}
      FMemoSQL  : TMemo ;
    {$ENDIF}

    procedure GUItoBOM ;
    procedure SetData(const Value: TSQLMgrQuery);
    procedure SetReadOnly(const Value: boolean);
    function  GetSQL: string;
    procedure CloneData( const pList : TList ) ;
    procedure DoOnChange(Sender: TObject);
    procedure InsertTextIntoEditor(const AText: string);
  protected
  public
    procedure Execute ;
    procedure ExecuteThenSave;
    property  Data : TSQLMgrQuery read FData write SetData ;
    property  ReadOnly : boolean read FReadOnly write SetReadOnly ;
    procedure LoadFromFile( pFileName : TFileName ) ;
    procedure SaveToFile(   pFileName : TFileName ) ;
    procedure Clear ;
    property  SQL : string read GetSQL ;
    procedure SetFocus ; override ;
    property  Dirty : boolean read FDirty write FDirty ;
  end;

implementation
uses
  FSQLMgrBrowse
  ,tiUtils
  ,ClipBrd
  ,Math
  ,tiDialogs
  ;


//{$IFDEF FPC}
  {$R *.dfm}
//{$ENDIF}
  

procedure TFormSQLEditor.Execute;
var
  lThrd : TthrdSQLMgrBrowse ;
  lData : TSQLMgrQuery ;
  lList : TList ;
begin
  lList := TList.Create ;
  try
    CloneData( lList ) ;
    lData := TSQLMgrQuery( lList.Items[0] ) ;
    lThrd := TthrdSQLMgrBrowse.Create(true) ;
    lThrd.SQLMgrQuery := lData ;
    lThrd.Start ;
  finally
    lList.Free ;
  end ;
end;

procedure TFormSQLEditor.ExecuteThenSave ;
var
  lThrd : TthrdSQLMgrSave ;
  lData : TSQLMgrQuery ;
  lList : TList ;
begin
  lList := TList.Create ;
  try
    CloneData( lList ) ;
    lData := TSQLMgrQuery( lList.Items[0] ) ;
    lThrd := TthrdSQLMgrSave.Create(true) ;
    lThrd.SQLMgrQuery := lData ;
    lThrd.Start ;
  finally
    lList.Free ;
  end ;
end;

procedure TFormSQLEditor.CloneData( const pList : TList ) ;
var
  lData : TSQLMgrQuery ;
begin
  pList.Clear ;

  GUIToBOM ;
  if FData <> nil then
    lData := FData.Clone
  else
  begin
    lData := TSQLMgrQuery.Create ;
    lData.SQL := FMemoSQL.Text ;
  end ;

  if FMemoSQL.SelText <> '' then
    lData.SQL := FMemoSQL.SelText ;

  pList.Add( lData ) ;

end;

procedure TFormSQLEditor.FormCreate(Sender: TObject);
begin

  {$IFDEF SYNEDIT}
    FMemoSQL := TSynEdit.Create( self ) ;
  {$ELSE}
    aFindInSQL.Visible := false ;
    aFindInSQLAgain.Visible := false ;
    FMemoSQL := TMemo.Create( self ) ;
    FMemoSQL.Font.Name := 'Courier New' ;
    FMemoSQL.Font.Size := 10 ;
    FMemoSQL.ScrollBars := ssBoth ;
  {$ENDIF}

  {$IFDEF SYNEDIT}
    FMemoSQL.Gutter.ShowLineNumbers := true ;
    FMemoSQL.Gutter.LeftOffset      := 0 ;

    FWMSync := TSynSQLSyn.Create( self ) ;
    with FWMSync do begin
      CommentAttri.Foreground    := clGray     ;
      CommentAttri.Style         := [fsItalic] ;
      IdentifierAttri.Foreground := clNavy     ;
      KeyAttri.Foreground        := clMaroon   ;
      KeyAttri.Style             := [fsBold]   ;
      NumberAttri.Foreground     := clGreen    ;
      StringAttri.Foreground     := clTeal     ;
      SymbolAttri.Foreground     := clRed      ;
    end ;

    FMemoSQL.Highlighter := FWMSync ;
    FSearchText := '' ;
  {$ENDIF}

  FMemoSQL.Parent := self ;
  FMemoSQL.Align := alClient ;
  FMemoSQL.PopupMenu := pmSQL ;

  FMemoSQL.OnKeyDown  := DoKeyDown ;
  FMemoSQL.OnKeyPress := DoKeyPress ;
  FMemoSQL.OnChange   := DoOnChange ;

  FbPickDatabaseObject := false ;

end;

procedure TFormSQLEditor.GUItoBOM;
var
  ls : string ;
begin
  ls := FMemoSQL.Lines.Text ;
  if ( FData <> nil ) and
     ( FData.SQL <> ls ) then
  begin
    FData.SQL := ls ;
    FData.Dirty := true ;
  end;
  FDirty := true ;
end;

procedure TFormSQLEditor.SetData(const Value: TSQLMgrQuery);
var
  lSL : TStringList ;
begin

  FData := Value;

  if FData = nil then
    Exit ; //==>

  FMemoSQL.OnChange          := nil ;

  // There is a bug in the MWCustomEdit which causes an extra line to be
  // inserted when a strings is assigned by using the text property.
  lSL := TStringList.Create ;
  try
    lsl.Text := FData.SQL ;
    FMemoSQL.Lines.Assign( lsl ) ;
  finally
    lsl.Free ;
  end ;

  FMemoSQL.OnChange := DoOnChange ;
  FDirty := false ;
  
end;

procedure TFormSQLEditor.aCopyToClipAsQuotedExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
  lsl : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    if FMemoSQL.SelText <> '' then
      lsl.Text := FMemoSQL.SelText
    else
      lsl.Assign( FMemoSQL.Lines ) ;

    ls := '' ;
    for i := 0 to lsl.Count - 1 do begin
      ls := ls + '    ' + QuotedStr( TrimRight(lsl.Strings[i]) + ' '  ) + ' ' ;
      if i < lsl.Count - 1 then
        ls := ls + '+' + CrLf
      else
        ls := ls + ';' ;
    end ;
  finally
    lsl.Free ;
  end ;
  Clipboard.AsText := LowerCase(ls) ;
end;

procedure TFormSQLEditor.aRunQueryExecute(Sender: TObject);
begin
  Execute ;
end;

procedure TFormSQLEditor.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  aRunQuery.Enabled    := Length( FMemoSQL.Text ) <> 0 ;
end;

procedure TFormSQLEditor.SetReadOnly(const Value: boolean);
var
  SQLColor: TColor;
begin
  FReadOnly := Value;
  FMemoSQL.ReadOnly                  := Value;
  if Value then SQLColor := clBtnFace
  else SQLColor := clWindow;

  FMemoSQL.Color                     := SQLColor;
  {$IFDEF SYNEDIT}
    FWMSync.IdentifierAttri.Background := SQLColor;
    FWMSync.KeyAttri.Background        := SQLColor;
    FWMSync.NumberAttri.Background     := SQLColor;
    FWMSync.StringAttri.Background     := SQLColor;
    FWMSync.SymbolAttri.Background     := SQLColor;
  {$ENDIF}
end;

procedure TFormSQLEditor.LoadFromFile(pFileName: TFileName);
begin
  FMemoSQL.Lines.LoadFromFile( pFileName ) ;
end;

procedure TFormSQLEditor.Clear;
begin
  FMemoSQL.Lines.Clear ;
end;

function TFormSQLEditor.GetSQL: string;
begin
  if FMemoSQL.SelText <> '' then
    result := FMemoSQL.SelText
  else
    result := FMemoSQL.Lines.Text ;
  result := Trim( result ) ;
end;

procedure TFormSQLEditor.SaveToFile(pFileName: TFileName);
begin
  FMemoSQL.Lines.SaveToFile( pFileName ) ;
end;

procedure TFormSQLEditor.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//var
//  lR: longint;
//  lSelStart : integer ;
//  lPoint : TPoint ;
begin
  exit;
(*
  if not (( Key = 32 ) and
          ( ssCtrl in Shift )) then
    Exit ; //==>

  Key := 0 ;
  FbPickDatabaseObject := true ;

  if FMemoSQL.Text = '' then
    FMemoSQL.Text := ' ' ;

  {$IFDEF SYNEDIT}
    lSelStart := 0 ;
  {$ELSE}
    lSelStart := FMemoSQL.SelStart ;
    if lSelStart = Length( FMemoSQL.Text ) then
      Dec( lSelStart ) ;
  {$ENDIF}

  lR := SendMessage(FMemoSQL.Handle, EM_POSFROMCHAR, lSelStart, 0);
  lPoint.Y := HiWord( lR ) ;
  lPoint.X := LoWord( lR ) ;
  // This will be forms canvas, so take care...
  lPoint.Y := lPoint.Y + Canvas.TextHeight( 'M' ) ;
  lPoint := FMemoSQL.ClientToScreen( lPoint ) ;

  if FFormPickDatabaseObject = nil then
    FFormPickDatabaseObject := TFormPickDatabaseObject.Create( nil ) ;

  lResult := FFormPickDatabaseObject.Execute( lPoint ) ;
  if lResult <> '' then
    InsertTextIntoEditor(lResult);
*)
end;


procedure TFormSQLEditor.InsertTextIntoEditor(const AText: string);
begin
  {$IFDEF SYNEDIT}
    FMemoSQL.SelText:= AText ;
  {$ELSE}
    SendMessage(
      FMemoSQL.Handle,
      EM_REPLACESEL,  // Replace current selection, if no selection then inserts
      0,              // undo option - 0 if undo not required, 1 if undo required
      Integer(PChar( AText ))); // Squeeze it into an integer for lparam
  {$ENDIF}
  GUItoBOM ;
end;

procedure TFormSQLEditor.FormDestroy(Sender: TObject);
begin
//  FFormPickDatabaseObject.Free ;
end;

procedure TFormSQLEditor.SetFocus;
begin
  inherited;
  FMemoSQL.SetFocus ;
end;

procedure TFormSQLEditor.DoKeyPress(Sender: TObject; var Key: Char);
begin
  if FbPickDatabaseObject then
  begin
    FbPickDatabaseObject := false ;
    Key := #0 ;
  end ;
  GUItoBOM ;
end;

procedure TFormSQLEditor.DoOnChange(Sender: TObject);
begin
  GUItoBOM ;
end;

procedure TFormSQLEditor.aFindInSQLExecute(Sender: TObject);
begin
  {$IFDEF SYNEDIT}
    if FMemoSQL.SelText <> '' then
      FSearchText := FMemoSQL.SelText;
    if tiInputQuery( FSearchText,
                     'What do you want to search for?',
                     'Search text:' ) then
      aFindInSQLAgainExecute( nil );
  {$ENDIF}
end;

procedure TFormSQLEditor.aFindInSQLAgainExecute(Sender: TObject);
begin
  {$IFDEF SYNEDIT}
    if FSearchText <> '' then
    begin
      if FMemoSQL.SearchReplace( FSearchText, '', []) = 0 then
      begin
        FMemoSQL.BlockBegin := FMemoSQL.BlockEnd;
        FMemoSQL.CaretXY    := FMemoSQL.BlockBegin;
      end
    end else
      aFindInSQLExecute(Nil);
  {$ENDIF}
end;

procedure TFormSQLEditor.aCopyToClipExecute(Sender: TObject);
var
  ls : string ;
begin
  if FMemoSQL.SelText <> '' then
    ls := FMemoSQL.SelText
  else
    ls := FMemoSQL.Lines.Text ;
  Clipboard.AsText := LowerCase(ls);
end;

procedure TFormSQLEditor.aPasteAsQuotedStringExecute(Sender: TObject);
  function _ParseLine(const ALine: string): string;
  const
    cApos = '&apos;';
  begin
    Result := Trim(ALine);
    if Copy(Result, Length(Result), 1) = '+' then
      Result := Copy(Result, 1, Length(Result)-1);
    if Copy(Result, Length(Result), 1) = ';' then
      Result := Copy(Result, 1, Length(Result)-1);
    Result := tiStrTran(Result, '''''', cApos);
    Result := tiStrTran(Result, '''', '');
    Result := tiStrTran(Result, cApos, '''');
  end;
var
  lsl : TStringList;
  i: Integer;
begin
  lsl := TStringList.Create;
  try
    lsl.Text := Clipboard.AsText;
    for i := 0 to lsl.Count - 1 do
      lsl.Strings[i] := _ParseLine(lsl.Strings[i]);
    InsertTextIntoEditor(lsl.Text);
  finally
    lsl.Free;
  end;
end;

initialization
{$IFDEF FPC}
  {$i FSQLEditor.lrs}
{$ENDIF}

end.
