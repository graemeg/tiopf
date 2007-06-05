unit FWorkList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiFormMgrForm, StdCtrls, ExtCtrls, tiHyperlinkWithImage;

const
  cFormCaption = 'Demonstration address book application';

type
  TFormWorkList = class(TFormTIFormMgrForm)
    tiHyperlinkWithImage1: TtiHyperlinkWithImage;
    imgTop: TImage;
    imgBottom: TImage;
    imgRight: TImage;
    memoHint: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure tiHyperlinkWithImage1Hint(Sender: TtiHyperlinkWithImage;
      const AHint: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWorkList: TFormWorkList;

implementation
uses
  FMain
 ;

{$R *.dfm}

procedure TFormWorkList.FormCreate(Sender: TObject);
begin
  inherited;
  FormCaption:= cFormCaption;
  ButtonsVisible:= btnVisNone;
  UpdateButtons:= False;
  tiHyperlinkWithImage1.Action:= FormMain.aViewAddressList;
end;

procedure TFormWorkList.tiHyperlinkWithImage1Hint(Sender: TtiHyperlinkWithImage;
  const AHint: string);
begin
  inherited;
  MemoHint.Lines.Text:= AHint;
end;

end.
