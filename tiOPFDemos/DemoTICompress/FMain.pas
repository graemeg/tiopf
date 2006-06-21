{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    June 2000, Peter Hinrichsen, Created demonstration

  Purpose:
    To demonstrate the use of the TtiCompress and TtiCompressFactory

  ToDo:
    Write buffer demo

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls ;

type

  TFormMain = class(TForm)
    cbCompressionType: TComboBox;
    PC: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    eFileNameBefore: TEdit;
    eFileNameCompress: TEdit;
    eFileNameAfter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    bbTestFileCompression: TBitBtn;
    bbTestStringCompression: TBitBtn;
    mBefore: TMemo;
    mAfter: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    bbTestStreamCompression: TBitBtn;
    Label7: TLabel;
    eStreamBefore: TEdit;
    Label8: TLabel;
    eStreamCompress: TEdit;
    Label9: TLabel;
    eStreamAfter: TEdit;
    sbFileOpen: TSpeedButton;
    OD: TOpenDialog;
    Memo4: TMemo;
    cbLaunch: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure bbTestFileCompressionClick(Sender: TObject);
    procedure bbTestStringCompressionClick(Sender: TObject);
    procedure sbFileOpenClick(Sender: TObject);
    procedure eStreamBeforeChange(Sender: TObject);
    procedure bbTestStreamCompressionClick(Sender: TObject);
  private
    procedure CreateTextFile;
    function  GetString : string ;

  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses
  // You MUST use TtiCompressAbs in the unit which calls the factory
  tiCompressAbs

  // You MAY use tiCompressZLib in the application if you want to make
  // ZLib compression available. It is only necessary to use tiCompressZLib
  // in one place in the app.
  ,tiCompressZLib

  // You MAY use tiCompressNone in the application if you want to make
  // NullObject compression available. It is only necessary to use
  // TtiCompressNone in one place in the app.
  ,tiCompressNone

  // Necessary for ShellExecute( )
  ,ShellAPI
  ;

{$R *.DFM}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormMain
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Form's constructor
// -----------------------------------------------------------------------------
procedure TFormMain.FormCreate(Sender: TObject);
begin
  gCompressFactory.AssignCompressionTypes( cbCompressionType.Items ) ;
  cbCompressionType.ItemIndex := 0 ;
  mBefore.Text := GetString ;
  eStreamBefore.Text := 'CompressionTest.exe' ;
  PC.ActivePageIndex := 0 ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  File compression test
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Run the file compression test
// -----------------------------------------------------------------------------
procedure TFormMain.bbTestFileCompressionClick(Sender: TObject);
var
  lCompress : TtiCompressAbs ;
  lslBefore : TStringList ;
  lslAfter  : TStringList ;
  lrRatio   : real ;
begin

  CreateTextFile ;
  lCompress := gCompressFactory.CreateInstance( cbCompressionType.Items[cbCompressionType.ItemIndex] ) ;
  try
    lrRatio := lCompress.CompressFile(   eFileNameBefore.Text,
                                         eFileNameCompress.Text ) ;
    lCompress.DecompressFile( eFileNameCompress.Text,
                              eFileNameAfter.Text ) ;
  finally
    lCompress.Free ;
  end ;

  lslBefore := TStringList.Create ;
  lslAfter  := TStringList.Create ;
  try
    lslBefore.LoadFromFile( eFileNameBefore.Text ) ;
    lslAfter.LoadFromFile(  eFileNameAfter.Text ) ;
    if lslBefore.Text = lslAfter.Text then
      ShowMessage( Format( 'Test passed. Compressed size of %n%%',
                           [lrRatio]))
    else
      ShowMessage( 'Test failed' ) ;
  finally
   lslBefore.Free ;
   lslAfter.Free ;
  end ;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * Run the string compression test
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Create a string of 200,000 bytes size to compress
// -----------------------------------------------------------------------------
function TFormMain.GetString : string ;
var
  i      : integer ;
  lsLine : string ;
begin
  lsLine := '' ;
  for i := 1 to 1000 do
    lsLine := lsLine + Chr( ord('A')+random(ord('z')-ord('A'))) ;
  for i := 1 to 200 do
    result := result + lsLine + #13 + #10 ;
end ;

// Run the string compression test
// -----------------------------------------------------------------------------
procedure TFormMain.bbTestStringCompressionClick(Sender: TObject);
var
  lCompress    : TtiCompressAbs ;
  lsBefore     : string ;
  lsCompress   : string ;
  lsDecompress : string ;
  lrRatio      : real ;
begin

  // Create the appropriate TtiCompress concrete
  lCompress := gCompressFactory.CreateInstance( cbCompressionType.Items[cbCompressionType.ItemIndex] ) ;
  try
    // Get some text to compress
    lsBefore := mBefore.Text ;
    // Compress the text, returning the compression ratio
    lrRatio := lCompress.CompressString( lsBefore,
                                        lsCompress ) ;
    // Decompress the text
    lCompress.DecompressString( lsCompress,
                                lsDecompress ) ;
    // Assign the decompressed text to a memo
    mAfter.Text := lsDecompress ;
  finally
    lCompress.Free ;
  end ;

  // Test to see if the test passed
  if mBefore.Text = mAfter.Text then
    ShowMessage( Format( 'Test passed. Compressed size of %n%%',
                         [lrRatio]))
  else
    ShowMessage( 'Test failed' ) ;

end;

// Create a text file to compress
// -----------------------------------------------------------------------------
procedure TFormMain.CreateTextFile ;
var
  lsl    : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    lsl.Text := GetString ;
    lsl.SaveToFile( eFileNameBefore.Text ) ;
  finally
    lsl.Free ;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  Stream compression test
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

// Execute the FileOpen dialog
// -----------------------------------------------------------------------------
procedure TFormMain.sbFileOpenClick(Sender: TObject);
begin
  OD.FileName := eStreamBefore.Text ;
  if OD.Execute then
    eStreamBefore.Text := OD.FileName ;
end ;

// Set the StreamTest file names
// -----------------------------------------------------------------------------
procedure TFormMain.eStreamBeforeChange(Sender: TObject);
begin
  eStreamCompress.Text :=
    ChangeFileExt( eStreamBefore.Text, '.TMP' ) ;
  eStreamAfter.Text :=
    ChangeFileExt( eStreamBefore.Text, '' ) +
    '_After' +
    ExtractFileExt( eStreamBefore.Text ) ;
end;

// Run the stream compression test
// -----------------------------------------------------------------------------
procedure TFormMain.bbTestStreamCompressionClick(Sender: TObject);

  procedure TestCompressStream ;
  var
    lFrom : TFileStream ;
    lTo   : TFileStream ;
    lCompress : TtiCompressAbs ;
    lrRatio   : real ;
  begin
    lFrom := TFileStream.Create( eStreamBefore.Text, fmOpenRead or fmShareExclusive ) ;
    try
      lTo   := TFileStream.Create( eStreamCompress.Text, fmCreate or fmShareExclusive ) ;
      try
        lCompress := gCompressFactory.CreateInstance( cbCompressionType.Items[cbCompressionType.ItemIndex] ) ;
        try
          lrRatio := lCompress.CompressStream( lFrom, lTo ) ;
        finally
          lCompress.Free ;
        end ;
      finally
        lTo.Free ;
      end ;
    finally
      lFrom.Free ;
    end;
    ShowMessage( Format( 'Compressed size of %n%%',
                         [lrRatio])) ;
  end ;

  procedure TestDecompressStream ;
  var
    lFrom : TFileStream ;
    lTo   : TFileStream ;
    lCompress : TtiCompressAbs ;
  begin
    lFrom := TFileStream.Create( eStreamCompress.Text, fmOpenRead or fmShareExclusive ) ;
    try
      lTo   := TFileStream.Create( eStreamAfter.Text, fmCreate or fmShareExclusive ) ;
      try
        lCompress := gCompressFactory.CreateInstance( cbCompressionType.Items[cbCompressionType.ItemIndex] ) ;
        try
          lCompress.DecompressStream( lFrom, lTo ) ;
        finally
          lCompress.Free ;
        end ;
      finally
        lTo.Free ;
      end ;
    finally
      lFrom.Free ;
    end;
  end ;

begin

  Assert( FileExists( eStreamBefore.Text ),
          'File <' + eStreamBefore.Text + '> not found.' ) ;

  TestCompressStream ;
  TestDecompressStream ;

  if cbLaunch.Checked then

    ShellExecute( Application.MainForm.Handle,
                  nil,
                  PChar(eStreamAfter.Text),
                  nil,
                  nil,
                  SW_SHOWNORMAL )
  else
    ShowMessage( 'You can now test launching <' +
                 eStreamAfter.Text + '>' ) ;

end;

end.

