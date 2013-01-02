unit main;

// Code Example of using ADOM 4.3.3
// Delphi for .NET implementation
//
// You need ADOM 4.3.3 or above to use this source code.
// The latest version of ADOM can be found at "http://www.philo.de/xml/".
//
// This sample code demonstrates how to process an XML file and check its
// well-formedness. It also shows how to copy one XML node tree to another
// using some of ADOM's fundamental XML processor classes.

interface

uses
  DK.Adom_4_3.AdomCore_4_3,  
  Buttons, Controls, Dialogs, Forms, StdCtrls, System.ComponentModel;

type

  TForm1 = class(TForm)
    XmlStandardDocReader1: TXmlStandardDocReader;
    OpenFileBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    Memo1: TMemo;
    DomImplementation: TDomImplementation;
    XmlStandardDomReader1: TXmlStandardDomReader;
    BitBtn1: TBitBtn;
    Memo2: TMemo;
    XmlStandardHandler1: TXmlStandardHandler;
    ListBox1: TListBox;
    Label3: TLabel;
    XmlDistributor1: TXmlDistributor;
    DomToXmlParser1: TDomToXmlParser;
    ResResolver: TStandardResourceResolver;
    XmlDomBuilder1: TXmlDomBuilder;
    XmlDomBuilder2: TXmlDomBuilder;
    XmlWFTestHandler1: TXmlWFTestHandler;
    XmlNamespaceSignalGenerator1: TXmlNamespaceSignalGenerator;
    procedure OpenFileBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure DomImplementationError(Sender: TObject; Error: TdomError;
      var Continue: Boolean);
    procedure XmlStandardHandler1Signal(Sender: TObject;
      Signal: TXmlSignal; var Accept: Boolean);
  private
    { Private declarations }
    function GetXmlDoc1: TdomDocument;
    function GetXmlDoc2: TdomDocumentNS;
  public
    { Public declarations }
    property XmlDoc1: TdomDocument read getXmlDoc1;
    property XmlDoc2: TdomDocumentNS read getXmlDoc2;
  end;

var
  Form1: TForm1;

implementation

{$R *.nfm}

uses
  DK.Utilities.dkCodecUtilsRTL, DK.Utilities.dkUriUtils,
  Classes, SysUtils;

// TForm1
// ------

function TForm1.GetXmlDoc1: TdomDocument;
begin
  Result := (DomImplementation.Documents.Item(0) as TdomDocument);
end;

function TForm1.GetXmlDoc2: TdomDocumentNS;
begin
  Result := (DomImplementation.Documents.Item(1) as TdomDocumentNS);
end;

procedure TForm1.OpenFileBtnClick(Sender: TObject);
var
  Ok: Boolean;
  MStream: TMemoryStream;
  InputSrc: TXmlInputSource;
  S: string;
begin
  if OpenDialog.Execute then begin

    if not FileExists(OpenDialog.FileName) then begin
      Memo1.Text := 'File not found!';
      Exit;
    end;

    Memo1.Clear;
    Memo1.Update;

    XmlDoc1.Clear;
    XmlDomBuilder1.ReferenceNode := xmlDoc1;

    ListBox1.Items.BeginUpdate;
    try
      ListBox1.Clear;

      // Parse the XML File
      // ------------------
      InputSrc := nil;  // Setting InputSrc to nil saves us one try ... finally block below.
      MStream := TMemoryStream.Create; // First, we need a stream for the Input Source below.
      try
        MStream.LoadFromFile(OpenDialog.Filename);  // Here we load the entire contents of the XML file into the stream's memory buffer.
        InputSrc := TXmlInputSource.Create(
          MStream, '', FilenameToUriStr(OpenDialog.Filename, []), 4096, TUnicodeCodecClass(nil), True, 0, 0, 0, 0, 1); // Now, we create the Input Source.
        Ok := XmlStandardDocReader1.Parse(InputSrc); // Voila! We parse the Input Source.
      finally
        InputSrc.Free;
        MStream.Free;
      end; {try}

    finally
      ListBox1.Items.EndUpdate;
    end;

    if Ok then begin
      DomToXmlParser1.WriteToString(XmlDoc1, 'Latin1', S);
      Memo1.Text := S;
      Memo1.Lines.Add('');
      Memo1.Lines.Add('');
      Memo1.Lines.Add('OK.');
    end else begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('');
      Memo1.Lines.Add('TERMINATED.');
      ListBox1.Clear;
    end;
    Memo1.Update;

  end; 
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  Ok: boolean;
  S: string;
begin
    Memo2.Clear;
    Memo2.Update;

    XmlDoc2.Clear;
    XmlDomBuilder2.ReferenceNode := XmlDoc2;
    Ok := XmlStandardDomReader1.Parse(XmlDoc1);
    if Ok then begin
      DomToXmlParser1.WriteToString(XmlDoc2, 'Latin1', S);
      Memo2.Text := S;
      Memo2.Lines.Add('');
      Memo2.Lines.Add('');
      Memo2.Lines.Add('OK.');
    end else begin
      Memo2.Lines.Add('');
      Memo2.Lines.Add('');
      Memo2.Lines.Add('TERMINATED.');
    end;
    Memo2.Update;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TDomDocument.Create(DomImplementation);
  TDomDocumentNS.Create(DomImplementation);
end;

procedure TForm1.DomImplementationError(Sender: TObject; Error: TdomError;
  var Continue: Boolean);
begin
  if (Sender = XmlStandardDocReader1)
    then Memo1.Lines.Add('Error #' + IntToStr(Ord(Error.relatedException)))
    else Memo2.Lines.Add('Error #' + IntToStr(Ord(Error.relatedException)));
end;

procedure TForm1.XmlStandardHandler1Signal(Sender: TObject;
  Signal: TXmlSignal; var Accept: Boolean);
begin
  if Signal is TXmlStartElementSignal then
    with ListBox1.Items do
      if IndexOf(TXmlStartElementSignal(Signal).TagName) = -1  // If the tag name is not already present in the ListBox,
        then Add(TXmlStartElementSignal(Signal).TagName);      // then add the tag name to the ListBox.
end;

end.
