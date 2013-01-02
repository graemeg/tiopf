unit AdomPropertyEditor_4_3;

  {$IFDEF WIN32}
    {$IFNDEF VER140}
      {$DEFINE MSWINDOWS}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WIN16}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
  {$IFDEF VER140}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER150}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER160}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER170}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER180}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER185}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER190}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER200}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER210}
    {$DEFINE VER140+}
  {$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Forms, Dialogs, TypInfo,
  {$IFDEF LINUX}
    QStdCtrls, QButtons, QControls, DesignIntf, DesignEditors;
  {$ELSE}
    StdCtrls, Buttons, Controls,
    {$IFDEF VER140+} // Delphi 7 up
      {$IFDEF CLR}    
        Borland.Vcl.Design.DesignIntf, Borland.Vcl.Design.DesignEditors,
  System.ComponentModel;
        // If you encounter a compile error here, you must manually add
        // Borland.Studio.Vcl.Design.dll to the 'Requires' section of the
        // ADOM package by right-clicking on the 'Requires' folder and
        // selecting 'Add Reference...', etc.
      {$ELSE}
        DesignIntf, DesignEditors;
      {$ENDIF}
    {$ELSE} DsgnIntf; {$ENDIF}
  {$ENDIF}

type
  THandlerListEditor = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    CancelBtn: TBitBtn;
    AddBtn: TBitBtn;
    RemoveBtn: TBitBtn;
    ClearBtn: TBitBtn;
    UpBtn: TBitBtn;
    DownBtn: TBitBtn;
    OKBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  THandlerListProperty = class(TPropertyEditor)
  protected
    FHandlerListEditor: THandlerListEditor;
    procedure FOnGetStrProc(const S: string);
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
  end;

var
  HandlerListEditor: THandlerListEditor;

implementation

uses
  AdomCore_4_3;

{$IFDEF LINUX}
  {$R *.xfm}
{$ELSE}
  {$IFDEF CLR}
    {$R *.nfm}
  {$ELSE}
    {$R *.dfm}
  {$ENDIF}
{$ENDIF}

// ++++++++++++++++++++++++ THandlerListProperty ++++++++++++++++++++++++
procedure THandlerListProperty.FOnGetStrProc(const S: string);
begin
  if Assigned(FHandlerListEditor) then
    if FHandlerListEditor.ListBox1.Items.IndexOf(S) = -1
      then FHandlerListEditor.ListBox2.Items.AddObject(S, Designer.GetComponent(S));
end;

procedure THandlerListProperty.Edit;
var
  I: Integer;
  Distributor: TXmlDistributor;
  S: string;
begin
  FHandlerListEditor:= THandlerListEditor.Create(application);
  try
    Distributor:= GetComponent(0) as TXmlDistributor;
    with Distributor.NextHandlers do begin
      for I := 0 to Pred(count) do begin
        S := Designer.GetComponentName(Items[I].XmlHandler);
        if S <> '' then FHandlerListEditor.ListBox1.Items.AddObject(S, Items[I].XmlHandler);
      end;
    end;
    Designer.GetComponentNames(GetTypeData(TypeInfo(TXmlCustomHandler)), FOnGetStrProc);
    if FHandlerListEditor.ShowModal = mrOK then begin
      Distributor.NextHandlers.Assign(FHandlerListEditor.ListBox1.Items);
{$IFNDEF CLR}
      SetOrdValue(Longint(Distributor.NextHandlers));
{$ENDIF}
    end;
  finally
    FHandlerListEditor.Free;
  end;
end;

function THandlerListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadonly];
end;

{$IFDEF CLR}
function THandlerListProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType.Name]);
end;

{$ELSE}

function THandlerListProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;
{$ENDIF}



// +++++++++++++++++++++++++ THandlerListEditor +++++++++++++++++++++++++
procedure THandlerListEditor.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure THandlerListEditor.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure THandlerListEditor.AddBtnClick(Sender: TObject);
var
  I: integer;
begin
  with ListBox2 do begin
    Items.BeginUpdate;
    ListBox1.Items.BeginUpdate;
    for I := 0 to Pred(Items.Count) do
      if Selected[I] then
        ListBox1.Items.AddObject(Items.Strings[I], Items.Objects[I]);
    for I := pred(Items.Count) downto 0 do
      if Selected[I] then
        Items.Delete(I);
    Items.EndUpdate;
    ListBox1.Items.EndUpdate;
  end;
end;

procedure THandlerListEditor.RemoveBtnClick(Sender: TObject);
var
  I: integer;
begin
  with ListBox1 do begin
    Items.BeginUpdate;
    ListBox2.Items.BeginUpdate;
    for I := 0 to Pred(Items.Count) do
      if Selected[I] then
        ListBox2.Items.AddObject(Items.Strings[I], Items.Objects[I]);
    for I := Pred(Items.Count) downto 0 do
      if Selected[I] then
        Items.Delete(I);
    Items.EndUpdate;
    ListBox2.Items.EndUpdate;
  end;
end;

procedure THandlerListEditor.UpBtnClick(Sender: TObject);
var
  I: integer;
begin
  with ListBox1 do begin
    for I := 1 to Pred(Items.Count) do
      if Selected[I] then Items.Exchange(I, Pred(I));
  end;
end;

procedure THandlerListEditor.DownBtnClick(Sender: TObject);
var
  I: Integer;
begin
  with ListBox1 do begin
    for I := Items.Count - 2 downto 0 do
      if Selected[I] then Items.Exchange(I, Succ(I));
  end;
end;

procedure THandlerListEditor.ClearBtnClick(Sender: TObject);
var
  I: Integer;
begin
  with ListBox1 do begin
    Items.BeginUpdate;
    ListBox2.Items.BeginUpdate;
    for I := 0 to Pred(Items.Count) do
      ListBox2.Items.AddObject(Items.Strings[I], Items.Objects[I]);
    Clear;
    Items.EndUpdate;
    ListBox2.Items.EndUpdate;
  end;
end;

end.
