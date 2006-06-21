unit AstiHyperLink;

{To Do:
Property and methods to develop:
- HotTrackCursor property: Give possibility to users to select type of cursor
  when mouse go over the control
- Exposing inherited properties.
- Create integer property summing to Self.Width and Self.height to make area sensitive
  wider then now (hardcoded sum of 2 pixel)
- Create a property to align Text property Vertically, horizontally and center it  

NOTES:
- fSwapped private variable is necessary because we cannot trustee about sequence of
  VCL calls MouseMove override and DoMouseLeave method (CM_MOUSELEAVE message procedure).
- For Click method I did not need to write ShellExecute because I created a new
  public method GoHyperLink that owns ShellExecute call.
  
-
}

interface

uses
  SysUtils, Classes, Controls, Messages,
  tiFocusPanel, AsHyperLink, Windows, Graphics;

type
  TAstiHyperLink = class(TtiFocusPanel)
  private
    FHTFont: TFont;
    FText: string;
    FURL: string;
    fOK: TOpenKind;
    fSwapFont: TFont;
    fSwapped: Boolean;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetText(const Value: string);
    procedure SetURL(const Value: string);
  protected
    procedure DoMouseLeave; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure Click; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    {: This method can allow you to jump to hyperlink at runtime. If the control has the focus you can
       jump to hyperlink also with Enter key.}
    procedure GoHyperLink;
  published
    {: Setting OpenKind allow you to define how to open client application to browse hyperlink}
    property OpenKind: TOpenKind read fOK write fOK default okShow;
    property HotTrackFont: TFont read FHTFont write SetHotTrackFont;
    {: Text property replace the tipical caption property. It can be, of course, different from URL.
       Text and URL property are related such as Name and Text property of tipical TControl}
    property Text: string read FText write SetText;
    {: URL property keeps the hyperlink. It does not need to be only a classic web hyperlink, but also
       different type of hyperlink such as, for example,  mailto: a.sanguigni@samatel.com}
    property URL: string read FURL write SetURL;
   //inherited property
    property Font;
    property TabOrder;
    property TabStop Default True;
  end;

procedure Register;

implementation

uses ShellAPI;

procedure Register;
begin
  RegisterComponents('TechInsite', [TAstiHyperLink]);
end;

{ TAstiHyperLink }

constructor TAstiHyperLink.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  TabStop := True;
  fHTFont := TFont.Create;
  fHTFont.Color := clBlue;
  fHTFont.Style := fHTFont.Style + [fsBold,fsUnderline];
  Font.Color := clBlue;
  Font.Style := Font.Style + [fsUnderline];
  fOK := okShow;
  fUrl := 'http://www.techinsite.com.au';
  fText := fUrl;
  fSwapFont := TFont.Create;
  fSwapped := False;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TAstiHyperLink.Destroy;
begin
  fHTFont.Free;
  fSwapFont.Free;
  inherited Destroy;
end;

procedure TAstiHyperLink.Click;
begin
  inherited;
  GoHyperLink;
end;

procedure TAstiHyperLink.CMMouseLeave(var Msg: TMessage);
begin
  DoMouseLeave;
end;


procedure TAstiHyperLink.DoMouseLeave;
begin
  Cursor := crDefault;
  if fSwapped = True then
  begin
    Font.Assign(fSwapFont); //come back to prior property values
    fSwapped := False;
  end;
end;

procedure TAstiHyperLink.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then //if Enter then go hyperlink
    Click;
end;

procedure TAstiHyperLink.Loaded;
begin
  inherited;
  SetUrl(fUrl); //this method sets fUrl after completing the stream loading of DFM file
end;

procedure TAstiHyperLink.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Cursor := crHandPoint; { Show handpoint cursor }
  if fSwapped = False then
    fSwapFont.Assign(Font); //Assign Font to a Tfont swap temp object
  Font.Assign(fHTFont); //this assignment call FontChanged method that Invalidate control
  fSwapped := True;
  inherited MouseMove(Shift, X, Y);
end;

procedure TAstiHyperLink.Paint;
var
  PanelWidth, PanelHeigth: integer;
begin
  inherited Paint;
  PanelWidth  := Canvas.TextWidth(Text);
  PanelHeigth := Canvas.TextHeight(Text);
  Self.Width  := PanelWidth + 2;
  Self.Height := PanelHeigth + 2;
  Canvas.TextOut(1,1,Text);
end;

procedure TAstiHyperLink.SetHotTrackFont(const Value: TFont);
begin
  FHTFont.Assign(Value);
end;

procedure TAstiHyperLink.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Invalidate; //implicitly call Paint method to adjust panel size to Text string size
  end;
end;

procedure TAstiHyperLink.SetURL(const Value: string);
begin
  if not (csLoading in ComponentState) then
    if fUrl = Text then //simulate the Name - Text=Caption properties behaviour
      Text := Value;
  fUrl := Value;
end;

//Jump to hyperlink
procedure TAstiHyperLink.GoHyperLink;
begin
  ShellExecute(Parent.Handle,'Open',PChar(URL),nil,nil,Ord(fOK));
end;

end.
