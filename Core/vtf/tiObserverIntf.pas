unit tiObserverIntf;

{$i vtfDefines.inc}

interface

uses
  tiTypes;

type
  ItiObserver = interface(IInterface)
    ['{27572A0E-83D8-4AA5-A3B4-C22BD69ACEA3}']
    procedure Update(const Subject: IInterface; const NotifyType: TtiNotifyType);
  end;

implementation

end.


