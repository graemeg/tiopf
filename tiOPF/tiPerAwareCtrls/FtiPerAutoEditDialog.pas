
{$I tiDefines.inc}

unit FtiPerAutoEditDialog;

interface
uses
  tiPtnVisPerObj
  ,FtiPerEditDialog
  ;

type

  TFormTIPerAutoEditDialog = class( TFormTIPerEditDialog )
  private
  public
    procedure SetData(const Value: TPerObjAbs ); override ;
  end ;


implementation
uses
  tiUtils
  ,Classes
  ,tiPerAwareCtrls
  ,TypInfo
  ;

{ TFormTIPerAutoEditDialog }

procedure TFormTIPerAutoEditDialog.SetData(const Value: TPerObjAbs );
var
  lsl : TStringList ;
  i   : integer ;
  lpa : TtiPerAwareAbs ;
  lRow : integer ;
  lCol : integer ;
const
  cBorder = 6 ;
  cCtrlHeight = 21 ;
  cCtrlWidth  = 180 ;
  cMaxRowCount = 10 ;
begin

  inherited SetData( Value ) ;

  if RO.ReadOnly then
    Caption := ' View ' + Value.Caption
  else
    Caption := ' Edit ' + Value.Caption ;

  lsl := TStringList.Create ;
  lRow := 0 ;
  lCol := 0 ;
  try
    tiGetPropertyNames( TPersistent( Value ),
                        lsl,
                        ctkSimple + [tkVariant, tkEnumeration] ) ;
    for i := 0 to lsl.Count - 1 do
    begin
      lpa := TtiPerAwareEdit.Create( self ) ;
      lpa.Parent := self ;
      lpa.Top  := cBorder + ( cCtrlHeight + cBorder ) * lRow ;
      lpa.Left := cBorder + ( cBorder + cCtrlWidth ) * lCol ;
      lpa.Width := cCtrlWidth ;
      lpa.LinkToData( Value, lsl.Strings[i] ) ;
      lpa.Caption := lsl.Strings[i] ;
      lpa.ReadOnly := RO.ReadOnly ;
      Inc( lRow ) ;
      if lRow >= cMaxRowCount then
      begin
        lRow := 0 ;
        if i <> lsl.Count - 1 then
          Inc( lCol ) ;
      end ;
    end ;

    if lsl.Count - 1 > cMaxRowCount then
      lRow := cMaxRowCount ;

    Self.ClientHeight := ( cCtrlHeight + cBorder ) * lRow +
                         self.btnOK.Height + cBorder * 2 ;

    Self.ClientWidth := ( cCtrlWidth + cBorder ) * ( lCol + 1 )
                         + cBorder ;

  finally
    lsl.Free ;
  end ;

end;

end.
