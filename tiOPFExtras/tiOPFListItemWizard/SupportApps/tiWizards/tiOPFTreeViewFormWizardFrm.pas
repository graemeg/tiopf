{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Chris Latta, Data Solutions Pty Ltd
  for inclusion in the tiOPF (TechInsite Object Persistence Framework) from
    TechInsite Pty. Ltd.
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm
    EMail:           info@techinsite.com.au

  Please submit changes to tiOPF@techinsite.com.au

  Purpose: An Open Tools API Wizard for Delphi to assist in the creation
  of a unit defining a TPerObjList which owns TPerObjAbs items

  Revision History:
    April 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFTreeViewFormWizardFrm;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList, ToolsAPI, ComCtrls,
  Grids;

type
  TfrmWizard = class(TForm)
    ActionList1: TActionList;
    actGenerate: TAction;
    actGenOneFile: TAction;
    Image1: TImage;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Bevel1: TBevel;
    pagWizard: TPageControl;
    tabForm: TTabSheet;
    gbTreeNode: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edTreeNode: TEdit;
    edTreeNodeUnit: TEdit;
    gbForm: TGroupBox;
    Label4: TLabel;
    Label3: TLabel;
    Label12: TLabel;
    edFormName: TEdit;
    edFormCaption: TEdit;
    procedure actGenerateExecute(Sender: TObject);
    procedure actGenerateUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TfrmWizard }

procedure TfrmWizard.actGenerateExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmWizard.actGenerateUpdate(Sender: TObject);
begin
  actGenerate.Enabled := True;
end;

procedure TfrmWizard.FormCreate(Sender: TObject);
begin
  pagWizard.ActivePage := tabForm;
end;

end.

