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

{$I tiDefines.inc}

unit FEditParam;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FtiPerEditDialog, StdCtrls, Buttons, tiPerAwareCtrls, ExtCtrls, tiPtnVisPerObj,
  tiReadOnly, tiFocusPanel;

type
  TFormEditParam = class(TFormTIPerEditDialog)
    paeName: TtiPerAwareEdit;
    paeType: TtiPerAwareComboBoxStatic;
    paeValue: TtiPerAwareEdit;
    paeIsNull: TtiPerAwareCheckBox;
    paeNameAllCaps: TtiPerAwareCheckBox;
    paeValueAllCaps: TtiPerAwareCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure paeNameAllCapsChange(Sender: TObject);
  private
  protected
    procedure SetData(const Value: TPerObjAbs); override ;
    function  FormIsValid : boolean ; override ;
  public
    { Public declarations }
  end;

var
  FormEditParam: TFormEditParam;

implementation
uses
   tiQuery
  ,tiRegINI
  ;
  
{$R *.DFM}

{ TFormEditParam }

procedure TFormEditParam.FormCreate(Sender: TObject);
begin
  inherited;
  QueryFieldKindsToStrings( paeType.Items ) ;
  paeNameAllCaps.Value := gINI.ReadBool(Name, 'NameAllCaps', false);
  paeValueAllCaps.Value := gINI.ReadBool(Name, 'ValueAllCaps', false);
end;

function TFormEditParam.FormIsValid: boolean;
begin
  result :=
    ( paeName.Value <> '' ) and
    ( paeType.Value <> '' ) and
    ( paeValue.Value <> '' ) ;
end;

procedure TFormEditParam.SetData(const Value: TPerObjAbs);
begin
  inherited SetData( Value ) ;

  paeName.LinkToData(   DataBuffer, 'ParamName'    ) ;
  paeType.LinkToData(   DataBuffer, 'ParamTypeStr' ) ;
  paeValue.LinkToData(  DataBuffer, 'ParamValue'   ) ;
  paeIsNull.LinkToData( DataBuffer, 'IsNull'       ) ;

end;

procedure TFormEditParam.FormShow(Sender: TObject);
begin
  inherited;
  if Data.ObjectState = posCreate then
    paeName.SetFocus
  else
    paeValue.SetFocus ;
end;

procedure TFormEditParam.FormDestroy(Sender: TObject);
begin
  inherited;
  gINI.WriteBool(Name, 'NameAllCaps', paeNameAllCaps.Value);
  gINI.WriteBool( Name, 'ValueAllCaps', paeValueAllCaps.Value);
end;

procedure TFormEditParam.paeNameAllCapsChange(Sender: TObject);
begin
  inherited;
  if paeNameAllCaps.Value then
    paeName.CharCase := ecUpperCase
  else
    paeName.CharCase := ecNormal ;
  if paeValueAllCaps.Value then
    paeValue.CharCase := ecUpperCase
  else
    paeValue.CharCase := ecNormal ;
  DataBuffer.PropValue[paeName.FieldName] := paeName.Value ;
  DataBuffer.PropValue[paeValue.FieldName] := paeValue.Value ;
end;

end.

