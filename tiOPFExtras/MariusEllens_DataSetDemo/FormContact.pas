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
unit FormContact;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, tiPtnVisPerObj, Adrs_BOM, tiDataset, Mask, DBCtrls, Db;

type
  TFormContact = class(TForm)
    dsEAddrType: TDataSource;
    tbEAddrType: TTiDataset;
    dsData: TDataSource;
    tbData: TTiRecordDataset;
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    Label1: TLabel;
    EditAddrType: TDBLookupComboBox;
    Label2: TLabel;
    EditText: TDBEdit;
  public
    constructor Create(AOwner:TComponent); override;
    procedure SetData(ARecord:TPerObjAbs);
  end;

implementation

{$R *.DFM}

{ TFormEditEAdrs }

constructor TFormContact.Create(AOwner: TComponent);
begin
  inherited;
  tbEAddrType.ObjectList := gAdrsBook.AdrsTypes.FindByListName('EAdrs');
  tbEAddrType.Open;
end;

procedure TFormContact.SetData(ARecord:TPerObjAbs);
begin
  tbData.oRecord := ARecord;
  tbData.Open;
end;

end.
