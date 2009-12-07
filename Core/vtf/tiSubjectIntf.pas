unit tiSubjectIntf;

{$i vtfDefines.inc}

interface

uses
  tiTypes, tiObserverIntf;

type
  ItiSubject = interface(IInterface)
    ['{2D3579C9-A9AE-4787-8473-C7EF3F5AC4E4}']
    function GetNotifying: boolean;
    procedure Attach(const Observer: ItiObserver);
    procedure Detach(const Observer: ItiObserver);
    procedure Notify(const Sender: IInterface; const NotifyType: TtiNotifyType);
    property Notifying: boolean read GetNotifying;
  end;

implementation

end.

