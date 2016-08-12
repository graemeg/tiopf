unit FtiDialogAbs;

{$I tiDefines.inc}

interface

uses
 Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type
  TFormTiDialogAbs = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
  end;

implementation
uses
   tiUtils
  ,tiGUIINI
 ;


{$R *.lfm}

procedure TFormTiDialogAbs.FormCreate(Sender: TObject);
begin
  gGUIINI.ReadFormState(self);
end;

procedure TFormTiDialogAbs.FormDestroy(Sender: TObject);
begin
  gGUIINI.WriteFormState(self);
end;

procedure TFormTiDialogAbs.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
  ActiveControl := nil;
end;

end.
