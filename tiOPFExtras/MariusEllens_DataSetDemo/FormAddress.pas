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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FormAddress;

interface

uses
  Classes, Controls, Forms, StdCtrls, Buttons, Db, DBCtrls, Mask,
  tiPtnVisPerObj, Adrs_BOM, tiDataset;

type
  TFormAddress = class(TForm)
    dsData: TDataSource;
    tbData: TTiRecordDataset;
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    Label1: TLabel;
    EditAddrType: TDBLookupComboBox;
    Label2: TLabel;
    EditLines: TDBMemo;
    Label3: TLabel;
    DBEdit2: TDBEdit;
    Label4: TLabel;
    DBEdit3: TDBEdit;
    Label5: TLabel;
    DBEdit4: TDBEdit;
    Label6: TLabel;
    DBEdit5: TDBEdit;
    tbAddrType: TTiDataset;
    dsAddrType: TDataSource;
  public
    constructor Create(AOwner:TComponent); override;
    procedure SetData(ARecord:TPerObjAbs);
  end;

implementation

{$R *.DFM}


{ TFormEditAdrs }

constructor TFormAddress.Create(AOwner: TComponent);
begin
  inherited;
  tbAddrType.ObjectList := gAdrsBook.AdrsTypes.FindByListName('Adrs');
  tbAddrType.Open;
end;

procedure TFormAddress.SetData(ARecord:TPerObjAbs);
begin
  tbData.oRecord := ARecord;
  tbData.Open;
end;


end.
