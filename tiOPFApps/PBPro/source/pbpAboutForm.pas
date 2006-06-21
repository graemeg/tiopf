{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit pbpAboutForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  JclFileUtils, ComCtrls,

  pbpResources, ActnList;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    CloseButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    VersionLabel: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    RichEdit: TRichEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ActionList1: TActionList;
    CreditsAction: TAction;
    LicenseAction: TAction;
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure CreditsActionExecute(Sender: TObject);
    procedure LicenseActionExecute(Sender: TObject);
  private
  public
    procedure Execute;
  end;

implementation

{$R *.DFM}

procedure TAboutForm.FormCreate(Sender: TObject);
var
  FileVersionInfo: TJclFileVersionInfo;
begin
  FileVersionInfo := TJclFileVersionInfo.Create(ParamStr(0));
  try
    VersionLabel.Caption := 'Version ' + FileVersionInfo.BinFileVersion;
  finally
    FileVersionInfo.Free;
  end;
  RichEdit.Lines.Text := LicenseText;
end;

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.Execute;
begin
  ShowModal;
end;

procedure TAboutForm.CreditsActionExecute(Sender: TObject);
begin
  RichEdit.Lines.Text := CreditsText;
end;

procedure TAboutForm.LicenseActionExecute(Sender: TObject);
begin
  RichEdit.Lines.Text := LicenseText;
end;

end.
