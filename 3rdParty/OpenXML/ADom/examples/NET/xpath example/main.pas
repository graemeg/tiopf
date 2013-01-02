unit main;

// Code Example of using ADOM 4.3.3
// Delphi for .NET implementation
//
// You need ADOM 4.3.3 or above to use this source code.
// The latest version of ADOM can be found at "http://www.philo.de/xml/".
//
// This example source code shows how to use ADOM's XPath functions.

interface

uses
  DK.Adom_4_3.AdomCore_4_3, System.ComponentModel,
  Buttons, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, StdCtrls, Tabs;

type
  TMainpage = class(TForm)
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog: TOpenDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TabControl1: TTabControl;
    TabSet1: TTabSet;
    TreeView: TTreeView;
    XmlToDomParser: TXmlToDomParser;
    DomImplementation: TDomImplementation;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    XmlStandardDomReader: TXmlStandardDomReader;
    Label1: TLabel;
    Label2: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    XPathExpression: TXPathExpression;
    StandardResourceResolver: TStandardResourceResolver;
    XmlDomBuilder: TXmlDomBuilder;
    XmlNamespaceSignalGenerator: TXmlNamespaceSignalGenerator;
    procedure OpenFile(Sender: TObject);
    procedure TabSet1Click(Sender: TObject);
    procedure CloseFile(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EvaluateExpression(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure DomImplementationRequestXPathVariable(Sender: TXPathExpression;
      NamespaceURI, LocalName: string; var Value: TDomXPathCustomResult);
  private
    { Private-Deklarationen }
    procedure UpdateTreeView(const Doc: TDomCustomDocument);
  public
    { Public-Deklarationen }
  end;

var
  Mainpage: TMainpage;

implementation

{$R *.nfm}

uses
  SysUtils, TypInfo, Windows;

function GetDisplayText(const Node: TDomNode): string;
begin
  Result := Node.NodeName;
  if Node.NodeValue <> '' then
    Result := Result + ' [' + Node.NodeValue + ']';
  Result:= Result + ' (' + GetEnumName(TypeInfo(TdomNodeType), Integer(Node.NodeType)) + ') ';
  if Node.NodeType = ntText_Node then
    if TdomText(Node).isElementContentWhitespace then
      Result := Result + '-- Whitespace in element content';
end;

procedure TMainpage.UpdateTreeView(const Doc: TDomCustomDocument);

  procedure HandleNodeList(Parent:TTreeNode; DomNodeList:TDomNodeList);
  var
    I: Integer;
    DomNode: TDomNode;
    TN: TTreeNode;
  begin
    for I :=0 to pred(DomNodeList.Length) do begin
      DomNode := DomNodeList.Item(I);
      TN := Parent.Owner.AddChildObject(Parent, GetDisplayText(DomNode), DomNode);
      if Assigned(DomNode.ChildNodes)
        then HandleNodeList(TN, DomNode.ChildNodes);
    end;
  end;

var
  Root: TTreeNode;
  S: string;
begin
  // Quick and dirty: always build the tree completely.
  // Delphi 5 seems to have problems with this approach,
  // but I could not figure out why.
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    S := Concat(Doc.NodeName, ' (', GetEnumName(TypeInfo(TdomNodeType), Integer(Doc.NodeType)), ') ', Doc.Classname);
    Root:= Treeview.Items.AddObject(nil, S, Doc);
    HandleNodeList(Root, Doc.ChildNodes);
    TreeView.FullExpand;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TMainpage.OpenFile(Sender: TObject);
var
  ParsingTime, TransformationTime, UpTime, ValidationTime: Cardinal;
  Doc_1: TdomDocument;
  Doc_2: TdomDocumentXPath;
  Index: Integer;

  procedure ShowErrorMessage(S: string);
  begin
    MessageDlg(S, mtError, [mbOK], 0);
    Label3.Caption:= '---';
    Label5.Caption:= '---';
    Label1.Caption:= '---';
    Label7.Caption:= '---';
  end;

begin

// The XPath data model is based on XML Namespaces (cf. the introduction to
// the XPath spec. available at "http://www.w3.org").  XML Namespace
// conformance can only fully be checked by validating a document (cf. sec. 6
// of the "Namespaces in XML" spec. also available at "http://www.w3.org").
// Therefore, we uses a four step process here:
//   1. Parse the document into a "normal" XML tree.
//   2. Validate it.
//   3. Parse the "normal" document tree into a namespace-aware document tree.
//   4. Validate the namespace-aware object tree.
// NB: a) Since the XPath data model is based on XML Namespaces, XPath
//        works only with namespace-aware document trees.
//     b) Since the XPath data model does not include a document type
//        declaration node, the XmlDocBuilder.KeepDocumentTypeDecl flag must be
//        set to 'False'.
//     c) Since the XPath data model treats default attributes the same as
//        specified attributes, the XmlStandardDomReader.IgnoreUnspecified flag
//        must be set to 'False'.
//     d) Since in the XPath data model there are no attributes corresponding to
//        attributes that declare namespaces, the
//        XmlNamespaceSignalGenerator.SupressXmlns flag is set to 'True'.

  with TabSet1 do
    if TabIndex > -1
      then OpenDialog.InitialDir:= ExtractFileDir(TDomCustomDocument(Tabs.Objects[TabIndex]).DocumentUri);

  if OpenDialog.Execute then begin
    Update;

    if not FileExists (OpenDialog.FileName) then begin
      ShowErrorMessage('File not found!');
      Exit;
    end;

    with XmlToDomParser do begin
      UpTime := GetTickCount;
      try
        Doc_1 := ParseFile(OpenDialog.FileName, True);
        try
          ParsingTime := GetTickCount;
          if not Doc_1.ValidationAgent.ValidateDocument(erReplace) then begin
            ShowErrorMessage('Document is invalid!');
            Doc_1.Free;
            Exit;
          end;
          ValidationTime := GetTickCount;
          Doc_2 := TDomDocumentXPath.Create(Doc_1.DomImplementation);
          Doc_2.Clear;
          XmlDomBuilder.ReferenceNode := Doc_2;
          if not XmlStandardDomReader.Parse(Doc_1) then begin
            ShowErrorMessage('Conversion to XPath data model failed!');
            Doc_2.Free;
            Exit;
          end;
          TransformationTime := GetTickCount;
          Index := TabSet1.Tabs.AddObject(ExtractFileName(Doc_1.DocumentUri), Doc_2);
          TabSet1.TabIndex := Index;
        finally
          Doc_1.Free;
        end;
      except
        ShowErrorMessage('Document is not well-formed!');
        Exit;
      end;
      Label3.Caption:= Format('%d ms', [ParsingTime - UpTime]);
      Label5.Caption:= Format('%d ms', [ValidationTime - ParsingTime]);
      Label1.Caption:= Format('%d ms', [TransformationTime - ValidationTime]);
      Label7.Caption:= '---';
    end; {with ...}

  end; {if OpenDialog.Execute ...}
end;

procedure TMainpage.TabSet1Click(Sender: TObject);
var
  Doc: TDomCustomDocument;
begin
  Label3.Caption:= '---';
  Label5.Caption:= '---';
  Label1.Caption:= '---';
  Label7.Caption:= '---';
  TreeView.Items.Clear;
  if TabSet1.TabIndex > -1 then begin
    SpeedButton2.Enabled:= True;
    Doc := TabSet1.Tabs.Objects[TabSet1.TabIndex] as TDomCustomDocument;
    UpdateTreeView(Doc);
    Update;
  end;
end;

procedure TMainpage.CloseFile(Sender: TObject);
var
  Doc: TDomCustomDocument;
begin
  with TabSet1 do begin
    if TabIndex > -1 then begin
      Doc := TDomCustomDocument(Tabs.Objects[TabIndex]);
      Doc.Free;
      Tabs.Delete(TabIndex);
      if Tabs.Count = 0 then SpeedButton2.Enabled := False;
    end;
  end;
end;

procedure TMainpage.FormCreate(Sender: TObject);
begin
  Caption:= 'ADOM ' + DomImplementation.AdomVersion + ' -- XPath Test Example';
end;

procedure TMainpage.EvaluateExpression(Sender: TObject);
var
  UpTime: Cardinal; 
  I: Integer;
begin
  with XPathExpression do begin
    Expression := Edit1.Text;
    if Assigned(TreeView.Selected)
      then ContextNode := TDomNode(TreeView.Selected.Data)
      else ContextNode := nil;
    UpTime:= GetTickCount;
    Evaluate;
    Label7.Caption:= Format('%d ms', [GetTickCount - UpTime]);
    with ListBox1.Items do begin
      BeginUpdate;
      try
        Clear;
        if ResultLength > 0 then begin
          for I := 0 to Pred(ResultLength) do begin
            if ResultNode(I).NodeType = ntXPath_Namespace_Node
              then AddObject(GetDisplayText(ResultNode(I)), (ResultNode(I) as TdomXPathNamespace).OwnerElement)
              else AddObject(GetDisplayText(ResultNode(I)), ResultNode(I));
          end;
        end else AddObject(ResultAsWideString, nil);
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TMainpage.DomImplementationRequestXPathVariable(
  Sender: TXPathExpression; NamespaceURI, LocalName: string;
  var Value: TDomXPathCustomResult);
// This event handler demonstrates how to bind a value to an XPath variable.
// Here the variabel $ADOM-version, which is case-sensitive, is bound to
// the current ADOM version as a string value.
begin
  if (NamespaceURI = '') and (Localname = 'ADOM-version') then
    Value := TdomXPathStringResult.Create(DomImplementation.AdomVersion);
end;

procedure TMainpage.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  // Trigger the evaluation when the Return key was hit:
  if key = #13 then EvaluateExpression(Sender);
end;

end.
