{
  Purpose:
    Run an adhoc query as a script - good for multiple queries, separated by ;s
    Bit of a hack though to get multiple queries running.

  Classes:
    TFormRunAsScript - The form

  ToDo:
    Remove the need to shell out to a script engine. Better to run the quesies
    from within the SQLManager process.

}


unit FRunAsScript;

{$I tiDefines.inc}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, tiButtons, tiPerAwareCtrls, tiFocusPanel,
  tiMemoReadOnly, tiPerAwareCombosAbs, tiPerAwareFileCombos
  {$IFDEF FPC}
  ,LResources
  {$ENDIF}
  ;

type
  TFormRunAsScript = class(TForm)
    tiButtonPanel1: TtiButtonPanel;
    hcParams: TtiPerAwareComboBoxHistory;
    memoMacros: TtiMemoReadOnly;
    paeApplicationToRun: TtiPerAwarePickFile;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tiButtonPanel1Btn1Click(Sender: TObject);
    procedure tiButtonPanel1Btn2Click(Sender: TObject);
  private
    function GetAppToRun: string;
    function GetParams: string;
    { Private declarations }
  public
    property AppToRun : string read GetAppToRun ;
    property Params   : string read GetParams ;
  end;


implementation
uses
  tiUtils
  ,tiINI
  ,tiGUIINI
  ;

//{$IFDEF FPC}
  {$R *.dfm}
//{$ENDIF}
  

procedure TFormRunAsScript.FormCreate(Sender: TObject);
begin
  gGUIINI.ReadFormState( self ) ;
  paeApplicationToRun.Value := gINI.ReadString( name, 'AppToRun', '' ) ;
  hcParams.Value   := gINI.ReadString( name, 'Params',   '' ) ;
end;

procedure TFormRunAsScript.FormDestroy(Sender: TObject);
begin
  gGUIINI.WriteFormState( self ) ;
  gINI.WriteString( name, 'AppToRun', paeApplicationToRun.Value ) ;
  gINI.WriteString( name, 'Params',   hcParams.Value   ) ;
end;

function TFormRunAsScript.GetAppToRun: string;
begin
  result := paeApplicationToRun.Value ;
end;

function TFormRunAsScript.GetParams: string;
begin
  result := hcParams.Value ;
end;

procedure TFormRunAsScript.tiButtonPanel1Btn1Click(Sender: TObject);
begin
  ModalResult := mrOK ;
end;

procedure TFormRunAsScript.tiButtonPanel1Btn2Click(Sender: TObject);
begin
  ModalResult := mrCancel ;
end;

initialization
  {$i FRunAsScript.lrs}

end.
