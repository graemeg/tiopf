{ EC Software Help Suite (EHS)

  © 2000-2003 EC Software. All rights reserved.

  This product and it's source code is protected by patents, copyright laws and
  international copyright treaties, as well as other intellectual property
  laws and treaties. The product is licensed, not sold.

  The source code and sample programs in this package or parts hereof
  as well as the documentation shall not be copied, modified or redistributed
  without permission, explicit or implied, of the author.


  EMail: info@ec-software.com
  Internet: http://www.ec-software.com


  Registration of components in the Delphi IDE
  --------------------------------------------
  This unit registers one or more components of EHS. If you want to register
  all components included, simply compile it. To exclude components from
  registration, insert a dot before the $DEFINE EHS_xxx statement below.


  Disclaimer of Warranty
  ----------------------
  THIS SOFTWARE AND THE ACCOMPANYING FILES ARE PROVIDED "AS IS" AND
  WITHOUT WARRANTIES OF ANY KIND WHETHER EXPRESSED OR IMPLIED.

  In no event shall the author be held liable for any damages whatsoever,
  including without limitation, damages for loss of business profits,
  business interruption, loss of business information, or any other loss
  arising from the use or inability to use the software. }


  {$DEFINE EHS_WHATSTHIS}
  {$DEFINE EHS_HELPROUTER}
  {$DEFINE EHS_TRAININGCARD}

unit ehs_reg;

interface

uses SysUtils, Classes, Dialogs,
     {$IFDEF VER100} DsgnIntf {$ENDIF}  //D3
     {$IFDEF VER120} DsgnIntf {$ENDIF}  //D4
     {$IFDEF VER130} DsgnIntf {$ENDIF}  //D5 + BCB5
     {$IFDEF VER140} DesignIntf, DesignEditors {$ENDIF}  //D6 + BCB6
     {$IFDEF VER150} DesignIntf, DesignEditors {$ENDIF}  //D7
     ;

type
  { property editor for THelpContextMap.FileName }
  THPJFileNameProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;


procedure Register;

implementation

{$R *.dcr}

uses
{$IFDEF EHS_WHATSTHIS}
  ehswhatsthis,
{$ENDIF}
{$IFDEF EHS_HELPROUTER}
  ehshelprouter,
{$ENDIF}
{$IFDEF EHS_TRAININGCARD}
  ehstcard,
{$ENDIF}
  ehscontextmap;

procedure Register;
begin
{$IFDEF EHS_WHATSTHIS}
  RegisterComponents('EHS', [TWhatsThis, THelpContextMap]);
  RegisterPropertyEditor(TypeInfo(TFileName), THelpContextMap, 'FileName', THPJFileNameProperty);
{$ENDIF}
{$IFDEF EHS_HELPROUTER}
  RegisterComponents('EHS', [THelpRouter]);
{$ENDIF}
{$IFDEF EHS_TRAININGCARD}
  RegisterComponents('EHS', [TTrainingCard]);
{$ENDIF}
end;

{ THPJFileNameProperty editor }

procedure THPJFileNameProperty.Edit;
begin
     with TOpenDialog.Create(nil) do
     try
        FileName := GetValue;
        Filter := 'Help project files (*.hpj; *.hhp)|*.hpj;*.hhp|All files (*.*)|*.*';
        if execute then SetValue(filename);
     finally
        free;
     end;
end;

function THPJFileNameProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure THPJFileNameProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function THPJFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


end.
