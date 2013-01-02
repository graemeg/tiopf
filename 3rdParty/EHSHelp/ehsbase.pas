{ EC Software Help Suite

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

unit ehsbase;

{$I EHS.INC}
{$I ESHHelpWarn.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms;

const
  EHS_VERSION = 1.23;

type
  TehsBase = class(TComponent)  {base class with window handle}
  private
    fWindowHandle: HWND;
  public
    destructor Destroy; override;
    procedure  Loaded; override;
    procedure  WndProc(var Message: TMessage); virtual;
    property   Handle: HWND read fWindowHandle;
  end;

implementation

procedure TehsBase.Loaded;
begin
     inherited;
     if (not (csDesigning in ComponentState)) then
     begin
          {$IFDEF EHS_D6_UP}
          fWindowHandle := Classes.AllocateHWnd(WndProc);
          {$ELSE}
          fWindowHandle := AllocateHWnd(WndProc);
          {$ENDIF}
          if fWindowHandle = 0 then raise Exception.create('EHSBase component cannot create window handle.');
     end;
end;

destructor TehsBase.Destroy;
begin
     if (not (csDesigning in ComponentState)) and (fWindowHandle <> 0) then
     begin
          {$IFDEF EHS_D6_UP}
          Classes.DeallocateHWnd(fWindowHandle);
          {$ELSE}
          DeallocateHWnd(fWindowHandle);
          {$ENDIF}
     end;
     inherited;
end;

procedure TehsBase.WndProc(var Message: TMessage);
begin
     with Message do Result := DefWindowProc(fWindowHandle, Msg, wParam, lParam);
end;

end.
