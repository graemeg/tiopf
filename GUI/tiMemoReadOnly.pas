unit tiMemoReadOnly;

{$I tiDefines.inc}

interface
uses
  StdCtrls
  ,Classes
 ;

type
  // TtiMemoReadOnly
  TtiMemoReadOnly = class(TCustomMemo)
  private
  published
    property Align;
    {$IFNDEF FPC}property Alignment;{$ENDIF}
    property Anchors;
    property Top;
    property Left;
    property Height;
    property Width;
    property Lines;
    property WordWrap;
    property Font;
    property ParentFont;
    property ShowHint;
    property PopupMenu;
    property OnClick;
    property ScrollBars;
  public
    Constructor Create(AOwner : TComponent); override;
  end;


implementation
uses
  Forms
  ,Controls
 ;
  
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiMemoReadOnly
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiMemoReadOnly.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly   := true;
  ParentColor := true;
  WordWrap   := true;
  BorderStyle := bsNone;
  TabStop    := false;
end;

end.
