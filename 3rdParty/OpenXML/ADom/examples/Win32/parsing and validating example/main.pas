unit main;

// Code Example of using ADOM 4.3.0
// Delphi implementation
//
// You need ADOM 4.3.0 or above to use this source code.
// The latest version of ADOM can be found at "http://www.philo.de/xml/".
//
// This example source code shows how to load
// and  display  the source  of XML files and
// their entities using the ADOM Delphi unit.

interface

uses
  AdomCore_4_3, 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TypInfo, StdCtrls, Buttons, Tabs, FileCtrl, ExtCtrls, ComCtrls;

type
  TMainpage = class(TForm)
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TabControl1: TTabControl;
    Memo1: TMemo;
    TabSet1: TTabSet;
    MessageMemo: TMemo;
    TreeView1: TTreeView;
    XmlToDomParser1: TXmlToDomParser;
    DomImplementation1: TDomImplementation;
    GroupBox1: TGroupBox;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    DomToXmlParser1: TDomToXmlParser;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    StandardResourceResolver1: TStandardResourceResolver;
    OpenDialog1: TOpenDialog;
    procedure OpenFile(Sender: TObject);
    procedure TabSet1Click(Sender: TObject);
    procedure CloseFile(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ReportError(Sender: TObject; Error: TdomError;
      var Continue: Boolean);
  private
    { Private-Deklarationen }
    procedure UpdateTreeView(const Doc: TdomDocument);
  public
    { Public-Deklarationen }
  end;

var
  Mainpage: TMainpage;

implementation

{$R *.DFM}

procedure TMainpage.UpdateTreeView(const Doc: TdomDocument);

  procedure HandleNodeList(Parent: TTreeNode; DomNodeList: TDomNodeList);
  var
    I: Integer;
    DomNode: TDomNode;
    Tn: TTreeNode;
    Astr: string;
  begin
    for I := 0 to Pred(DomNodeList.Length) do
    begin
      DomNode := DomNodeList.Item(I);
      Astr := DomNode.NodeName;
      if DomNode.NodeValue <> '' then Astr := Astr + ' [' + DomNode.NodeValue + ']';
      Astr := Astr + ' (' + GetEnumName(TypeInfo(TdomNodeType), Integer(DomNode.NodeType)) + ') ';
      if DomNode.NodeType = ntText_Node then
        if TdomText(DomNode).IsElementContentWhitespace then
          Astr := Astr + '-- Whitespace in element content';
      Tn := Parent.Owner.AddChildObject(Parent, Astr, DomNode);
      if Assigned(DomNode.ChildNodes) then HandleNodeList(Tn, DomNode.ChildNodes);
    end;
  end;

var
  Root: TTreeNode;
begin
  // Quick and dirty: always build the tree completely.
  // Delphi 5 seems to have problems with this approach,
  // but I could not figure out why.
  Treeview1.Items.BeginUpdate;
  try
    Treeview1.Items.Clear;
    Root := Treeview1.Items.AddObject(nil, Concat(Doc.NodeName,' (',GetEnumName(TypeInfo(TdomNodeType), Integer(Doc.NodeType)), ') ', Doc.Classname), Doc);
    HandleNodeList(Root,Doc.ChildNodes);
  finally
    Treeview1.Items.EndUpdate;
  end;
end;

procedure TMainpage.OpenFile(Sender: TObject);
var
  UpTime: {$ifdef VER100} Integer; {$else} Cardinal; {$endif} // use Integer in D3
  Index: Integer;
  DurationStr: string;
begin
  OpenDialog1.InitialDir := ExtractFileDir(Label1.Caption);
  if OpenDialog1.Execute then begin
    Update;

    if not FileExists (OpenDialog1.FileName) then begin
      Label3.Caption := '';
      Label6.Caption := '';
      MessageMemo.Text := 'File not found!';
      Exit;
    end;

    Memo1.Clear;
    Memo1.Update;
    MessageMemo.Clear;
    MessageMemo.Update;
    MessageMemo.Lines.BeginUpdate;
    try
      with XmlToDomParser1 do begin
        BufferSize := StrToInt(RadioGroup2.Items[RadioGroup2.ItemIndex]);
        UpTime := GetTickCount;
        try
          ParseFile(OpenDialog1.FileName, True);
          DurationStr := Format('%d ms', [GetTickCount - UpTime]);
          Index := TabSet1.Tabs.Add(ExtractFileName(OpenDialog1.FileName));
          TabSet1.TabIndex:= Index;
        except
          DurationStr := Format('%d ms', [GetTickCount - UpTime]);
          MessageMemo.Lines.Append('Document parsing abandoned.');
        end;
        Label3.Caption := DurationStr;
      end; {with ...}
    finally
      MessageMemo.Lines.EndUpdate;
    end;
    if MessageMemo.Text = '' then MessageMemo.Text:= 'Document successfully parsed.';

  end; {if OpenDialog1.Execute ...}
end;

procedure TMainpage.TabSet1Click(Sender: TObject);
var
  Doc: TdomDocument;
  DurationStr: string;
  S: string;
  UpTime: Cardinal;
begin
  with Memo1 do begin
    Clear;
    Update;
  end;
  Label1.Caption := '';
  Label3.Caption := '';
  Label6.Caption := '';
  Treeview1.Items.Clear;
  if TabSet1.TabIndex > -1 then begin
    SpeedButton2.Enabled := True;
    Doc := (XmlToDomParser1.DOMImpl.Documents.Item(TabSet1.TabIndex) as TdomDocument);
    Label1.Caption:= Doc.DocumentUri;

    Update;
    DomToXmlParser1.BufferSize := StrToInt(RadioGroup3.Items[RadioGroup3.ItemIndex]);
    UpTime := GetTickCount;
    DomToXmlParser1.WriteToString(Doc, 'Latin1', S);
    DurationStr := Format('%d ms', [GetTickCount - UpTime]);
    Label6.Caption := DurationStr;
    with Memo1 do begin
      Text := S;
      Update;
    end;

    UpdateTreeView(Doc);
    Update;
  end;
end;

procedure TMainpage.CloseFile(Sender: TObject);
var
  Doc: TdomDocument;
begin
  MessageMemo.Clear;
  if TabSet1.TabIndex > -1 then begin
    with TabSet1 do begin
      Doc := (XmlToDomParser1.DOMImpl.documents.Item(TabIndex) as TdomDocument);
      with XmlToDomParser1.DOMImpl do begin
        Doc.Free;
      end;
      Tabs.Delete(TabIndex);
      if Tabs.Count = 0 then SpeedButton2.Enabled := False;
    end;
  end;
end;

procedure TMainpage.TabControl1Change(Sender: TObject);
begin
  case TabControl1.TabIndex of
    0: begin Memo1.show; TreeView1.hide; Panel1.hide; end;
    1: begin Memo1.hide; TreeView1.show; Panel1.hide; end;
    2: begin Memo1.hide; TreeView1.hide; Panel1.show; end;
  end;
end;

procedure TMainpage.Button1Click(Sender: TObject);
var
  Doc: TdomDocument;
  EROpt: TdomEntityResolveOption;
begin
  Doc := (XmlToDomParser1.DOMImpl.Documents.Item(TabSet1.TabIndex) as TdomDocument);
  if Assigned(Doc) then begin
    if RadioGroup1.ItemIndex = 0
      then EROpt := erReplace
      else EROpt := erExpand;
    MessageMemo.Clear;
    if Doc.ValidationAgent.ValidateDocument(EROpt)
      then MessageDlg('No violations of validity constraints detected.', mtInformation, [mbOK], 0)
      else MessageDlg('Document is invalid!', mtInformation, [mbOK], 0);
    // Scroll Memo to the top:
    MessageMemo.Perform(WM_VSCROLL, SB_THUMBPOSITION, 0);
  end else
    MessageDlg('No active document!', mtError, [mbOK], 0);
  TabSet1Click(Sender);
end;

procedure TMainpage.FormCreate(Sender: TObject);
begin
  Caption:= 'ADOM ' + DomImplementation1.AdomVersion + ' Example: Parsing and Validating';
end;

procedure TMainpage.ReportError(Sender: TObject; Error: TdomError;
  var Continue: Boolean);
var
  ErrorStr, FileNameStr, NodeStr, PosStr, SeverityStr: string;

  function ExtractFileNameFromUri(const Uri: WideString): WideString;
  var
    I: Integer;
  begin
    If Uri = '' then
      Result := ''
    else begin
      I := LastDelimiter('/', Uri);
      Result := Copy(Uri, I + 1, MaxInt);
    end;
  end;

begin
  with Error do begin
    case Severity of
      DOM_SEVERITY_FATAL_ERROR: SeverityStr := 'Fatal Error';
      DOM_SEVERITY_ERROR:       SeverityStr := 'Error';
      DOM_SEVERITY_WARNING:     SeverityStr := 'Warning';
    end;

    FileNameStr := ExtractFileNameFromUri(Uri);
    if EndLineNumber = -1 then PosStr := ''
    else if StartLineNumber = EndLineNumber then begin
      if StartColumnNumber = EndColumnNumber
        then FmtStr(PosStr, '%d:%d', [EndLineNumber, EndColumnNumber])
        else FmtStr(PosStr, '%d:%d-%d', [EndLineNumber, StartColumnNumber, EndColumnNumber]);
    end else begin
      FmtStr(PosStr, '%d:%d-%d:%d', [StartLineNumber, StartColumnNumber, EndLineNumber, EndColumnNumber]);
    end;

    if Assigned(RelatedDtdObject) then begin
      NodeStr := Concat(' -- AS-NodeName: ', RelatedDtdObject.Name);
    end else if Assigned(RelatedNode) then begin
      NodeStr := Concat(' -- NodeName: ', RelatedNode.NodeName);
    end else NodeStr := '';

    ErrorStr := 'Error #' + IntToStr(Ord(RelatedException));

  end;

  if PosStr <> ''
    then MessageMemo.Lines.Add(Format('[%s] %s(%s): %s%s', [SeverityStr, FileNameStr, PosStr, ErrorStr, NodeStr]))
    else MessageMemo.Lines.Add(Format('[%s] %s: %s%s', [SeverityStr, FileNameStr, ErrorStr, NodeStr]));
end;

end.
