{ TTrainingCard

  © 2000-2003 EC Software. All rights reserved.

  This product and it's source code is protected by patents, copyright laws and
  international copyright treaties, as well as other intellectual property
  laws and treaties. The product is licensed, not sold.

  The source code and sample programs in this package or parts hereof
  as well as the documentation shall not be copied, modified or redistributed
  without permission, explicit or implied, of the author.


  EMail: info@ec-software.com
  Internet: http://www.ec-software.com

  Disclaimer of Warranty
  ----------------------

  THIS SOFTWARE AND THE ACCOMPANYING FILES ARE PROVIDED "AS IS" AND
  WITHOUT WARRANTIES OF ANY KIND WHETHER EXPRESSED OR IMPLIED.

  In no event shall the author be held liable for any damages whatsoever,
  including without limitation, damages for loss of business profits,
  business interruption, loss of business information, or any other loss
  arising from the use or inability to use the software. }

unit ehstcard;

interface

{$I ehs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TTrainingCardButtonEvent = procedure(Sender : TObject; Button: word) of object;
  TTrainingCardDataEvent = procedure(Sender : TObject; Data: integer) of object;

  TTrainingCard = class(TComponent)
  private
     fWindowHandle: HWND;
     fHelpFile: string;
     fIsActive: boolean;
     fOnCardData: TTrainingCardDataEvent;
     fOnButton: TTrainingCardButtonEvent;
     fOnClose: TNotifyEvent;
     fOnOtherCaller: TNotifyEvent;
     procedure SetHelpFile(newfile: string);
     procedure TrainingCardEvent(Action, Data: integer);
  protected
     procedure WndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Close;
    function    HelpCommand(Command: Word; Data: Longint): Boolean;
    function    HelpContext(Context: THelpContext): Boolean;
    function    HelpJump(const JumpID: string): Boolean;
    property    IsActive: boolean read fIsActive;
  published
    property HelpFile: string read fHelpFile write SetHelpFile;
    property OnCardData: TTrainingCardDataEvent read fOnCardData write fOnCardData;
    property OnButton: TTrainingCardButtonEvent read fOnButton write fOnButton;
    property OnClose: TNotifyEvent read fOnClose write fOnClose;
    property OnOtherCaller: TNotifyEvent read fOnOtherCaller write fOnOtherCaller;
  end;

implementation

constructor TTrainingCard.Create(AOwner: TComponent);
begin
     inherited;
     fWindowHandle := 0;
     fIsActive := false;
end;

destructor TTrainingCard.Destroy;
begin
     if fIsActive then Close;
     inherited;
end;

procedure TTrainingCard.SetHelpFile(newfile: string);
begin
     if fIsActive then Close;
     fHelpFile := newfile;
end;

procedure TTrainingCard.WndProc(var Message: TMessage);
begin
     with Message do
     begin
          if msg = WM_TCARD then TrainingCardEvent(wparam, lparam)
          else DefWindowProc(fWindowHandle, msg, wparam, lparam);
     end;
end;

procedure TTrainingCard.TrainingCardEvent(Action, Data: integer);
begin
     case Action of
     HELP_TCARD_DATA: if assigned(fOnCardData) then fOnCardData(self, Data);

     IDABORT, IDCANCEL,
     IDHELP, IDIGNORE,
     IDOK, IDNO,
     IDRETRY, IDYES: if assigned(fOnButton) then fOnButton(self, Action);

{ fIsActive is true when the user has closed the training card.
  It is false if we closed it programmatically with TTrainingCard.Close }

     IDCLOSE: if fIsActive then
              begin
                if assigned(fOnClose) then fOnClose(self);
                Close;
              end;
     HELP_TCARD_OTHER_CALLER:
       begin
            if assigned(fOnOtherCaller) then fOnOtherCaller(self);
            Close;
       end;
     end;
end;

function TTrainingCard.HelpCommand(Command: Word; Data: Longint): Boolean;
begin
     result := false;
     if (fHelpFile <> '') then
     begin
          if (not fIsActive) and (Command <> HELP_QUIT) then
          begin
                {$IFDEF EHS_D6_UP}
                fWindowHandle := Classes.AllocateHwnd(WndProc);
                {$ELSE}
                fWindowHandle := AllocateHwnd(WndProc);
                {$ENDIF}
          end;
          fIsActive := (Command <> HELP_QUIT) and (fWindowHandle <> 0);
          if fIsActive then
          begin
               WinHelp(fWindowHandle, PChar(fHelpFile), HELP_FORCEFILE + HELP_TCARD, 0);
               result := WinHelp(fWindowHandle, PChar(fHelpFile), Command + HELP_TCARD, Data);
          end;
     end;
end;

function TTrainingCard.HelpJump(const JumpID: string): Boolean;
var
  Command: array[0..255] of Char;
begin
  StrLFmt(Command, SizeOf(Command) - 1, 'JumpID("","%s")', [JumpID]);
  result := HelpCommand(HELP_COMMAND, Longint(@Command));
end;

function TTrainingCard.HelpContext(Context: THelpContext): Boolean;
begin
  result := HelpCommand(HELP_CONTEXT, Context);
end;

procedure TTrainingCard.Close;
begin
     if fWindowHandle <> 0 then
     begin
          WinHelp(fWindowHandle, PChar(fHelpFile), HELP_QUIT + HELP_TCARD, 0);
          {$IFDEF EHS_D6_UP}
          Classes.DeAllocateHwnd(fWindowHandle);
          {$ELSE}
          DeAllocateHwnd(fWindowHandle);
          {$ENDIF}
          fWindowHandle := 0;
          fIsActive := false;
     end;
     Application.HelpCommand(HELP_QUIT, 0);
end;

end.
