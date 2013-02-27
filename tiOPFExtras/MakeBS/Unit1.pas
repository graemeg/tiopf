(******************************************************************************

	MakeBS

	A utility to ease the job of creating _BOM and _SVR source files for
	your tiOPF business classes.

	I'm not a Delphi 'guru', so please be kind in your critiques. :)

	Written by Ray Jensen and donated to the tiOPF.

	Version History:
	- March 27, 2004     - Alpha Version 0.1  - creates a bare skeleton only
	- April 16, 2004     - Alpha Version 0.2  - added field generation code

******************************************************************************)

unit Unit1;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
	ExtCtrls, Buttons, Grids
	, tiPersist
	, tiPtnVisPerObj
	, tiUtils
	, uFields, Mask;

type

	TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
		Memo1: TMemo;
		TabSheet3: TTabSheet;
		Memo2: TMemo;
		TabSheet4: TTabSheet;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtClassName: TEdit;
    edtOutputDir: TEdit;
    btnBrowse: TButton;
    edtTableName: TEdit;
    Label7: TLabel;
    chkUseClassName: TCheckBox;
    Panel2: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    cmbFieldType: TComboBox;
    rgrVisibility: TRadioGroup;
    edtFieldName: TEdit;
    btnAddField: TBitBtn;
    btnRemove: TBitBtn;
    btnRemoveAll: TBitBtn;
    stgFields: TStringGrid;
    grpDirectives: TGroupBox;
    chkUseGetter: TCheckBox;
    chkUseSetter: TCheckBox;
    chkOverride: TCheckBox;
    chkReintroduce: TCheckBox;
    Memo3: TMemo;
    Panel3: TPanel;
    btnPreview: TButton;
    btnSave: TButton;
    btnExportCSV: TButton;
    btnImportCSV: TButton;
		procedure btnBrowseClick(Sender: TObject);
		procedure btnGenerateClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
		procedure FormKeyPress(Sender: TObject; var Key: Char);
		procedure DoCheckKeyPress(Sender: TObject; var Key: Char);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure btnAddFieldClick(Sender: TObject);
    procedure edtFieldNameChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure edtClassNameChange(Sender: TObject);
    procedure chkUseClassNameClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure stgFieldsClick(Sender: TObject);
    procedure btnRemoveAllClick(Sender: TObject);
    procedure edtFieldNameKeyPress(Sender: TObject; var Key: Char);
    procedure btnExportCSVClick(Sender: TObject);
    procedure btnImportCSVClick(Sender: TObject);
	private
		{ Private declarations }
		FAppDir: String;
		Fields: TbsFields;
		procedure ParseBOM;
		procedure ParseSVR;
		procedure DoBadThing;
		procedure RefreshFieldList;
	public
		{ Public declarations }
	end;

var
	Form1: TForm1;

implementation

uses
	FileCtrl, TypInfo, StrUtils, IniFiles;

{$R *.dfm}

var
	slBOM, slSVR: TStringList;
	ini: TIniFile;

const
	PAD_WIDTH = 28;
	sq = #39;

	CL: String = #13#10;
	T1: String = #9;
	T2: String = #9#9;
	T3: String = #9#9#9;
	S_OUTPUTDIR = 'OutputDir';

function PadQuoted(const s: String; len: Integer): String;
var
	ls : string ;
begin
	Assert(Length(s) > 0);

	ls := sq + s + sq;

	while length( ls ) < len do
		ls := ls + ' ' ;

	Result := ls ;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
	FTabWidth: Integer;
	i: Integer;
begin
	inherited;

	FAppDir := ExtractFileDir(Application.ExeName);

	slBOM := TStringList.Create;
	slSVR := TStringList.Create;

	ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	if ini.SectionExists('Settings') then
		edtOutputDir.Text := ini.ReadString('Settings', S_OUTPUTDIR, FAppDir)
	else
		edtOutputDir.Text := FAppDir;

	Fields := TbsFields.Create;

	edtClassName.Text := 'Customers';
	edtTableName.Text := 'Customers';

	PageControl1.Pages[2].TabVisible := False;
	PageControl1.Pages[3].TabVisible := False;
	PageControl1.ActivePageIndex := 0;

	FTabWidth := 8;
	SendMessage(Memo1.Handle, EM_SETTABSTOPS, 1, Longint(@FTabWidth));
	SendMessage(Memo2.Handle, EM_SETTABSTOPS, 1, Longint(@FTabWidth));

	for i := Ord(Low(TtiTypeKind)) to Ord(High(TtiTypeKind)) do
	begin
		cmbFieldType.Items.Add( GetEnumName(TypeInfo(TtiTypeKind), i) );
	end;
	{ set Integer as default field type }
	cmbFieldType.ItemIndex := 2;

	with stgFields do
	begin
		RowCount := 2;
		FixedRows := 1;

		ColWidths[0] := 140;
		ColWidths[1] := 60;
		ColWidths[2] := 60;
		ColWidths[3] := 50;
		ColWidths[4] := 50;
		ColWidths[5] := 50;
		ColWidths[6] := 50;

		Cells[0,0] := 'Name';
		Cells[1,0] := 'Type';
		Cells[2,0] := 'Scope';
		Cells[3,0] := 'Getter';
		Cells[4,0] := 'Setter';
		Cells[5,0] := 'O''ride';
		Cells[6,0] := 'Reint.';
	end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
	edtClassName.SetFocus;
  edtClassName.SelectAll;

	btnPreview.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
	ini.WriteString('Settings', S_OUTPUTDIR, edtOutputDir.Text);
	ini.Free;

	if Assigned(slBOM) then slBOM.Free;
	if Assigned(slSVR) then slSVR.Free;
	inherited;
end;

procedure TForm1.btnBrowseClick(Sender: TObject);
var
	dir: String;
begin
	dir := '';
	if (SelectDirectory(dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0)) and (dir <> '') then
		if DirectoryExists(dir) then
			edtOutputDir.Text := dir + '\';
end;

procedure TForm1.btnGenerateClick(Sender: TObject);
begin
	if (not DirectoryExists(edtOutputDir.Text)) then
	begin
		ShowMessage('Please set the output directory.');
		Exit;
	end;
	try
		slBOM.SaveToFile(edtOutputDir.Text + edtClassName.Text + '_BOM.pas');
		slSVR.SaveToFile(edtOutputDir.Text + edtClassName.Text + '_SVR.pas');
		ShowMessage('Files saved OK.' +
			#13#10+ edtOutputDir.Text + edtClassName.Text + '_BOM.pas' +
			#13#10+ edtOutputDir.Text + edtClassName.Text + '_SVR.pas');
	except
		raise;
	end;
end;

procedure TForm1.btnPreviewClick(Sender: TObject);
begin
	ParseBOM;
	ParseSVR;

	Memo1.Clear;
	Memo2.Clear;
	Memo1.Lines.AddStrings(slBOM);
	Memo2.Lines.AddStrings(slSVR);

	PageControl1.Pages[2].TabVisible := True;
	PageControl1.Pages[3].TabVisible := True;
	PageControl1.ActivePageIndex := 2;

	btnSave.Enabled := True;

end;

procedure TForm1.ParseBOM;
var
	CN,s,sProps,sFuncImp,fn,ft: String;
	i: Integer;
begin
	if not Assigned(slBOM) then DoBadThing;

	{ do the class name replacements }
	CN := edtClassName.Text;

	{ add the unit header stuff }

	s := 'unit ' + CN + '_BOM;                                                 '+CL+
		' '                                                                       +CL+
		'interface                                                               '+CL+
		' '                                                                       +CL+
		'uses Classes, tiPtnVisPerObj;                                           '+CL+
		' '                                                                       +CL+
		'type                                                                    '+CL+
		'  T'+CN+'       = class;                                                '+CL+
		'  T'+CN+'_List  = class;                                                '+CL+
		' '                                                                       +CL+
		'  { I use this for my TVirtualTreeviews }                               '+CL+
		'  PT'+CN+'Rec = ^T'+CN+'Rec;                                            '+CL+
		'  T'+CN+'Rec = record                                                   '+CL+
		'    OID: Integer;                                                       '+CL+
		'    Level: Integer;                                                     '+CL+
		'    pObject: TPerObjAbs;                                                '+CL+
		'  end;'                                                                  +CL+CL;

	{ add the TPerObjAbs class }

	s := s +
		'  T'+CN+' = class(TPerObjAbs)                                           '+CL+
		'  private                                                               '+CL;

	(* PRIVATE fields ----------------------------------------------------------*)
	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		fn := Fields[i].FieldName;
		ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
		System.Delete(ft, 1, 4); //drop leading 'tiTK' for readability
		if Fields[i].FieldType = tiTKDateTime then ft := 'T' + ft; //add for TDateTime types
		sProps := sProps +
			'    F' + fn + ': ' + ft + ';' + CL;
	end;
	s := s + sProps;

	s := s +
		'  protected                                                             '+CL+
		'    function GetCaption: String; override;                              '+CL;

	(* PROTECTED fields --------------------------------------------------------*)
	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		if Fields[i].Visibility = bsProtected then
		begin
			fn := Fields[i].FieldName;
			ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
			System.Delete(ft, 1, 4); //drop leading 'tiTK'
			if Fields[i].FieldType = tiTKDateTime then ft := 'T' + ft; //add for TDateTimes

			{ build up each field Getter/Setter function calls }
			if Fields[i].UseGetter then
				sProps := sProps +
					'    function Get' + fn + ': ' + ft + ';' ;

			if Fields[i].UseSetter then
				sProps := sProps +
					'    procedure Set' + fn + '( const Value: ' + ft + ');' ;

			if Fields[i].IsOverride then sProps := sProps + ' override;';
			if Fields[i].IsReintroduce then sProps := sProps + ' reintroduce;';
			sProps := sProps +CL;
		end;
	end;
	s := s + sProps;

	s := s +
		'  public                                                                '+CL+
		'    constructor Create; override;                                       '+CL+
		'    destructor Destroy; override;                                       '+CL+
		'    function Equals(pSource: T'+CN+'): Boolean; reintroduce;            '+CL+
		'    function Clone: T'+CN+'; reintroduce;                               '+CL+
		'    function IsValid(const pErrors: TPerObjErrors): Boolean; override;  '+CL;

	(* PUBLIC fields -----------------------------------------------------------*)
	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		if Fields[i].Visibility = bsPublic then
		begin
			fn := Fields[i].FieldName;
			ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
			System.Delete(ft, 1, 4); //drop leading 'tiTK'
			if Fields[i].FieldType = tiTKDateTime then ft := 'T' + ft;
			sProps := sProps +
				'    property ' + fn + ': ' + ft +
				' read '  + IfThen(Fields[i].UseGetter, 'Get' + fn + ' ', 'F' + fn ) +
				' write ' + IfThen(Fields[i].UseSetter, 'Set' + fn + ' ', 'F' + fn ) + ';' ;
			sProps := sProps +CL;
		end;
	end;
	s := s + sProps;

	s := s +
		'  published                                                             '+CL+
		'    property Caption: String read GetCaption;                           '+CL;

	(* PUBLISHED fields --------------------------------------------------------*)
	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		if Fields[i].Visibility = bsPublished then
		begin
			fn := Fields[i].FieldName;
			ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
			System.Delete(ft, 1, 4);
			if Fields[i].FieldType = tiTKDateTime then ft := 'T' + ft;
			sProps := sProps +
				'    property ' + fn + ': ' + ft +
				' read '  + IfThen(Fields[i].UseGetter, 'Get' + fn, 'F' + fn ) +
				' write ' + IfThen(Fields[i].UseSetter, 'Set' + fn, 'F' + fn ) + ';' + CL;
		end;
	end;
	s := s + sProps + '  end;' +CL+CL;

	{ add the TPerObjList class }

	s := s +
		'  T'+CN+'_List = class(TPerObjList)                                     '+CL+
		'  private                                                               '+CL+
		'    //                                                                  '+CL+
		'  protected                                                             '+CL+
		'    function GetItems(Index: Integer): T'+CN+' ; reintroduce ;          '+CL+
		'    procedure SetItems(Index: Integer; const Value: T'+CN+'); reintroduce ; '+CL+
		'  public                                                                '+CL+
		'    constructor Create(); override;                                     '+CL+
		'    property Items[Index: Integer] : T'+CN+' read GetItems write SetItems ; default; '+CL+
		'    procedure Add(pObject: T'+CN+'; pDefDispOrdr: Boolean = True) ; reintroduce ; '+CL+
		'  end;                                                                  '+CL+CL;

	{ add the implementation part }

	s := s +
		'implementation                                                          '+CL+
		'                                                                        '+CL+
		'{ T'+CN+' }                                                             '+CL+
		'                                                                        '+CL+
		'function T'+CN+'.Clone: T'+CN+';                                        '+CL+
		'begin                                                                   '+CL+
		'  Result := T'+CN+'.Create;                                             '+CL+
		'  Result.AssignPublishedProps(Self);                                    '+CL+
		'end;                                                                    '+CL+
		'                                                                        '+CL+
		'constructor T'+CN+'.Create;                                             '+CL+
		'begin                                                                   '+CL+
		'  inherited;                                                            '+CL+
		'//                                                                      '+CL+
		'end;                                                                    '+CL+
		'                                                                        '+CL+
		'destructor T'+CN+'.Destroy;                                             '+CL+
		'begin                                                                   '+CL+
		'//                                                                      '+CL+
		'  inherited;                                                            '+CL+
		'end;                                                                    '+CL+CL;

	{ add other methods }

	s := s +
    'function T'+CN+'.Equals(pSource: T'+CN+'): Boolean;                    '+CL+
		'begin                                                                  '+CL+
    '  Result := True;                                                      '+CL+
		'  Result := Result and (OID = pSource.OID) '                            +CL;

	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		fn := Fields[i].FieldName;
		ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
		System.Delete(ft, 1, 4);
		if Fields[i].FieldType = tiTKDateTime then ft := 'T' + ft;
		sProps := sProps +
		  '                   and ('+ fn +        ' = pSource.' + fn + '  )       '+CL;
	end;
	s := s + sProps +CL;

	s := s +
		'end;                                                                   '+CL+
    '                                                                       '+CL+
    'function T'+CN+'.GetCaption: String;                                   '+CL+
    'begin                                                                  '+CL+
		'  Result := '+sq+'Caption'+sq+';'                                       +CL+
    'end;                                                                   '+CL+
    '                                                                       '+CL+
    'function T'+CN+'.IsValid(const pErrors: TPerObjErrors): Boolean;       '+CL+
    'begin                                                                  '+CL+
		'  Result := True;                                                      '+CL+
		'  Result := Result and (OID   <> 0 )'                                   +CL+
		'end;                                                                   '+CL+CL;

	sFuncImp := '';
	for i := 0 to Fields.Count -1 do
	begin
		if Fields[i].UseGetter then
		begin
			fn := Fields[i].FieldName;
			ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
			System.Delete(ft, 1, 4);
			if Fields[i].FieldType = tiTKDateTime then ft := 'T' + ft;
			sFuncImp := sFuncImp +
				'function T' + CN + '.Get' + fn + ': ' + ft +';' +CL+
				'begin' +CL+
				'  Result := F' +fn+ ';' +CL+
				'end;' +CL+CL;

			{ build Setter function }
			sFuncImp := sFuncImp +
				'procedure T' + CN + '.Set' + fn + '(const Value: ' + ft +');' +CL+
				'begin' +CL+
				'  F' + fn + ' := Value;' +CL+
				'end;' +CL+CL;

		end;
	end;
	s := s + sFuncImp;

	s := s +	
		'{ T'+CN+'_List }                                                         '+CL+
		'                                                                         '+CL+
		'procedure T'+CN+'_List.Add(pObject: T'+CN+'; pDefDispOrdr: Boolean);     '+CL+
		'begin                                                                    '+CL+
		'  inherited Add(pObject);                                                '+CL+
		'end;                                                                     '+CL+
		'                                                                         '+CL+
		'constructor T'+CN+'_List.Create;                                         '+CL+
		'begin                                                                    '+CL+
		'  inherited;                                                             '+CL+
		'//                                                                       '+CL+
		'end;                                                                     '+CL+
		'                                                                         '+CL+
		'function T'+CN+'_List.GetItems(Index: Integer): T'+CN+';                 '+CL+
		'begin                                                                    '+CL+
		'  Result := T'+CN+'( inherited GetItems(Index) );                        '+CL+
		'end;                                                                     '+CL+
		'                                                                         '+CL+
		'procedure T'+CN+'_List.SetItems(Index: Integer; const Value: T'+CN+');   '+CL+
		'begin                                                                    '+CL+
		'  List[Index] := Value;                                                 '+CL+
		'end;                                                                     '+CL+
		'                                                                         '+CL+
		'end.                                                                     '+CL;

	{ assign to the BOM stringlist }
	slBOM.Text := s;

end;

procedure TForm1.ParseSVR;
var
	TN,CN,s,sProps,fn,fnq,ft: String;
	i: Integer;
begin
	if not Assigned(slSVR) then DoBadThing;

	{ class name replacements }
	CN := edtClassName.Text;
	TN := edtTableName.Text;

	s := 'unit '+CN+'_Svr;                                                    '+CL+CL+
		'interface                                                              '+CL+CL+
		'uses tiPtnVisSQL;                                                      '+CL+CL+
		'type                                                                   '+CL+CL+
		'  T'+CN+'_Read = class( TVisOwnedQrySelect )                           '+CL+
		'  protected                                                            '+CL+
		'    function  AcceptVisitor  : Boolean ; override ;                    '+CL+
		'    procedure Init           ; override ;                              '+CL+
		'    procedure SetupParams    ; override ;                              '+CL+
		'    procedure MapRowToObject ; override ;                              '+CL+
		'  end ;                                                                '+CL+
		'                                                                       '+CL+
		'  T'+CN+'_Create = class( TVisOwnedQryUpdate )                         '+CL+
		'  protected                                                            '+CL+
		'    function  AcceptVisitor  : Boolean ; override ;                    '+CL+
		'    procedure Init           ; override ;                              '+CL+
		'    procedure SetupParams    ; override ;                              '+CL+
		'  end ;                                                                '+CL+
		'                                                                       '+CL+
		'  T'+CN+'_Update = class( TVisOwnedQryUpdate )                         '+CL+
		'  protected                                                            '+CL+
		'    function  AcceptVisitor  : Boolean ; override ;                    '+CL+
		'    procedure Init           ; override ;                              '+CL+
		'    procedure SetupParams    ; override ;                              '+CL+
		'  end ;                                                                '+CL+
		'                                                                       '+CL+
		'  T'+CN+'_Delete = class( TVisOwnedQryUpdate )                         '+CL+
		'  protected                                                            '+CL+
		'    function  AcceptVisitor  : Boolean ; override ;                    '+CL+
		'    procedure Init           ; override ;                              '+CL+
		'    procedure SetupParams    ; override ;                              '+CL+
		'  end ;                                                                '+CL+
		'                                                                       '+CL+
		'implementation                                                         '+CL+
		'                                                                       '+CL+
		'uses                                                                   '+CL+
		'  '+CN+'_BOM                                                           '+CL+
		'  ,tiPersist                                                           '+CL+
		'  ,tiPtnVisPerObj                                                      '+CL+
		'  ;                                                                    '+CL+
    '                                                                       '+CL+
    '{ T'+CN+'_Read }                                                       '+CL+
		'                                                                       '+CL+
		'function T'+CN+'_Read.AcceptVisitor: Boolean;                          '+CL+
		'begin                                                                  '+CL+
		'  Result := ( Visited is T'+CN+'_List ) and                            '+CL+
		'            ( Visited.ObjectState = posEmpty ) ;                       '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Read.Init;                                           '+CL+
		'begin                                                                  '+CL+
		'  Query.SQL.Text :=                                                    '+CL+
		'    ' +sq+ 'select * from '+TN+ sq+ ';                                 '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Read.MapRowToObject;                                 '+CL+
		'var                                                                    '+CL+
		'  lData: T'+CN+';                                                      '+CL+
		'begin                                                                  '+CL+
		'  lData := T'+CN+'.Create;                                             '+CL+
		'  lData.OID := Query.FieldAsInteger [''OID''' + ' ];'                   +CL;

	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		fn := Fields[i].FieldName;
		ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
		System.Delete(ft, 1, 4);
		if Fields[i].FieldType = tiTKDateTime then ft := 'T' + ft;
		sProps := sProps + '  lData.' + tiPadR(fn, PAD_WIDTH) + ' := Query.' ;
		case Fields[i].FieldType of
			tiTKInteger  : sProps := sProps + 'FieldAsInteger  [' + PadQuoted(fn, PAD_WIDTH) + ' ]; ' +CL;
			tiTKFloat    : sProps := sProps + 'FieldAsFloat    [' + PadQuoted(fn, PAD_WIDTH) + ' ]; ' +CL;
			tiTKString   : sProps := sProps + 'FieldAsString   [' + PadQuoted(fn, PAD_WIDTH) + ' ]; ' +CL;
			tiTKDateTime : sProps := sProps + 'FieldAsDateTime [' + PadQuoted(fn, PAD_WIDTH) + ' ]; ' +CL;
			tiTKBoolean  : sProps := sProps + 'FieldAsBoolean  [' + PadQuoted(fn, PAD_WIDTH) + ' ]; ' +CL;
			else           sProps := sProps + '***BAD***       [' + PadQuoted(fn, PAD_WIDTH) + ' ]; ' +CL;
		end;
	end;
	s := s + sProps +
		'  lData.ObjectState := posClean;                                       '+CL+
		'  TPerObjList(Visited).Add(lData);                                     '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Read.SetupParams;                                    '+CL+
		'begin                                                                  '+CL+
		'	 inherited;                                                           '+CL+
		'//                                                                     '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'{ T'+CN+'_Create }                                                     '+CL+
		'                                                                       '+CL+
		'function T'+CN+'_Create.AcceptVisitor: Boolean;                        '+CL+
		'begin                                                                  '+CL+
		'  Result := ( Visited is T'+CN+' ) and                                 '+CL+
		'	           ( Visited.ObjectState = posCreate ) ;                      '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Create.Init;                                         '+CL+
		'begin                        	                                        '+CL+
		'  Query.SQL.Text :=                                                    '+CL+
		'    ' +sq+ 'insert into '+TN+ ' ( '+sq+' +'                             +CL+
		'    ' +sq+ '  OID '+sq +' + '                                           +CL;

	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		sProps := sProps +'    '+sq+ ' ,' + tiPadR(Fields[i].FieldName, PAD_WIDTH) +sq+ ' +' +CL;
	end;

	s := s + sProps +'    '+sq+' )'+sq+' +'                                    +CL+
		'  '+sq+'  Values (' +sq+' +'                                            +CL+
		'    '+sq+'  :OID  ' +sq+' +'                                            +CL;

	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		fn := tiPadR(Fields[i].FieldName, PAD_WIDTH);
		sProps := sProps + '    '+sq+', :' + fn +sq+ ' +' +CL;
	end;

	s := s + sProps +sq+ '   ) ' +sq +CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Create.SetupParams;                                  '+CL+
		'var                                                                    '+CL+
		'  lData : T'+CN+';                                                     '+CL+
		'begin                                                                  '+CL+
		'	 lData := Visited as T'+CN+';                                         '+CL+
		'	 Query.ParamAsInteger  [ ''OID''' + ' ] := lData.OID;                 '+CL;

	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		fnq := PadQuoted(Fields[i].FieldName, PAD_WIDTH);
		fn  := Fields[i].FieldName;
		sProps := sProps + '   Query.' ;
		case Fields[i].FieldType of
			tiTKInteger  : sProps := sProps + 'ParamAsInteger  [ ' + fnq + ' ] := lData.' + fn + ' ;'+CL ;
			tiTKFloat    : sProps := sProps + 'ParamAsFloat    [ ' + fnq + ' ] := lData.' + fn + ' ;'+CL ;
			tiTKString   : sProps := sProps + 'ParamAsString   [ ' + fnq + ' ] := lData.' + fn + ' ;'+CL ;
			tiTKDateTime : sProps := sProps + 'ParamAsDateTime [ ' + fnq + ' ] := lData.' + fn + ' ;'+CL ;
			tiTKBoolean  : sProps := sProps + 'ParamAsBoolean  [ ' + fnq + ' ] := lData.' + fn + ' ;'+CL ;
			else           sProps := sProps + '***BAD***       [ ' + fnq + ' ] := lData.' + fn + ' ;'+CL ;
		end;
	end;

	s := s + sProps +
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'{ T'+CN+'_Update }                                                     '+CL+
		'                                                                       '+CL+
		'function T'+CN+'_Update.AcceptVisitor: boolean;                        '+CL+
		'begin                                                                  '+CL+
		'  Result := ( Visited is T'+CN+' ) and                                 '+CL+
		'            ( Visited.ObjectState = posUpdate ) ;                      '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Update.Init;                                         '+CL+
		'begin                                                                  '+CL+
		'  Query.SQL.Text := '                                                   +CL+
		'    '+sq+ 'update '+TN+' Set '+sq +' +'                                 +CL;

	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		fn := tiPadR(Fields[i].FieldName, PAD_WIDTH);
		sProps := sProps + '     '+sq+ IfThen(i > 0, ',', ' ') + fn + ' = : ' + fn +sq+' +'+ CL;
	end;

	s := s + sProps +
		'    '+sq+'where '+sq+' +'                                               +CL+
		'    '+sq+'  OID = :OID; '+sq                                            +CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Update.SetupParams;                                  '+CL+
		'var                                                                    '+CL+
		'  lData : T'+CN+';                                                     '+CL+
		'begin                                                                  '+CL+
		'  lData := Visited as T'+CN+';                                         '+CL+
		'                                                                       '+CL+
		'  Query.ParamAsInteger [''OID''' + ' ] := lData.OID ; '+CL;

	sProps := '';
	for i := 0 to Fields.Count -1 do
	begin
		fnq := PadQuoted(Fields[i].FieldName, PAD_WIDTH);
		fn  := tiPadR(Fields[i].FieldName, PAD_WIDTH);
		sProps := sProps + '  Query.' ;
		case Fields[i].FieldType of
			tiTKInteger  : sProps := sProps + 'ParamAsInteger  [ ' + fnq + ' ] := lData.' + fn +' ;'+CL ;
			tiTKFloat    : sProps := sProps + 'ParamAsFloat    [ ' + fnq + ' ] := lData.' + fn +' ;'+CL ;
			tiTKString   : sProps := sProps + 'ParamAsString   [ ' + fnq + ' ] := lData.' + fn +' ;'+CL ;
			tiTKDateTime : sProps := sProps + 'ParamAsDateTime [ ' + fnq + ' ] := lData.' + fn +' ;'+CL ;
			tiTKBoolean  : sProps := sProps + 'ParamAsBoolean  [ ' + fnq + ' ] := lData.' + fn +' ;'+CL ;
			else           sProps := sProps + '***BAD***       [ ' + fnq + ' ] := lData.BAD ;'      +CL ;
		end;
	end;

	s := s + sProps +
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'{ T'+CN+'_Delete }                                                     '+CL+
		'                                                                       '+CL+
		'function T'+CN+'_Delete.AcceptVisitor: boolean;                        '+CL+
		'begin                                                                  '+CL+
		'  Result := ( Visited is T'+CN+' ) and                                 '+CL+
		'            ( Visited.ObjectState = posDelete ) ;                      '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Delete.Init;                                         '+CL+
		'begin                                                                  '+CL+
		'  Query.SQL.Text :=                                                    '+CL+
		'    '+sq+'delete from '+TN+' where OID = :OID;'+sq                      +CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'procedure T'+CN+'_Delete.SetupParams;                                  '+CL+
		'var                                                                    '+CL+
		'  lData : T'+CN+';                                                     '+CL+
		'begin                                                                  '+CL+
		'  lData := Visited as T'+CN+';                                         '+CL+
		'  Query.ParamAsInteger[''OID''' + '        ] := lData.OID;             '+CL+
		'end;                                                                   '+CL+
		'                                                                       '+CL+
		'initialization                                                         '+CL+
		'	 gTIPerMgr.RegReadVisitor( T'+CN+'_Read    );                         '+CL+
		'	 gTIPerMgr.RegSaveVisitor( T'+CN+'_Create  );                         '+CL+
		'	 gTIPerMgr.RegSaveVisitor( T'+CN+'_Update  );                         '+CL+
		'	 gTIPerMgr.RegSaveVisitor( T'+CN+'_Delete  );                         '+CL+
		'                                                                       '+CL+
		'finalization                                                           '+CL+
		'                                                                       '+CL+
		'end.                                                                   '+CL;

	slSVR.Text := s;
end;

procedure TForm1.DoBadThing;
begin
	ShowMessage('OOPS! A Big, Nasty Bug has just eaten the code... :( ');
	Halt;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
	btnPreview.Enabled := (edtClassName.Text <> '') and
												(edtOutputDir.Text <> '') ;
end;

procedure TForm1.DoCheckKeyPress(Sender: TObject; var Key: Char);
begin
	case Key of
		'A'..'Z','a'..'z','0'..'9',#8,#9,'_': ;
		#27:  TEdit(Sender).Perform(WM_UNDO, 0, 0);
		else Key := #0;
	end;
end;

procedure TForm1.btnAddFieldClick(Sender: TObject);
begin
	if Fields.IsField(edtFieldName.Text) then
	begin
		ShowMessage('Already Added.');
		SysUtils.Beep;
		Exit;
	end;

	with Fields.Add do
	begin
		FieldName     := edtFieldName.Text;
		FieldType     := TtiTypeKind(cmbFieldType.ItemIndex);
		Visibility    := TbsVisibility(rgrVisibility.ItemIndex);
		IsOverride    := chkOverride.Checked;
		IsReintroduce := chkReintroduce.Checked;
		UseGetter     := chkUseGetter.Checked;
		UseSetter     := chkUseSetter.Checked;
	end;

	RefreshFieldList;
end;

procedure TForm1.RefreshFieldList;
var
	i: Integer;
	sVis,ft: String;
begin

	btnRemove.Enabled := Fields.Count > 0;
	btnRemoveAll.Enabled := Fields.Count > 0;

	with stgFields do
	begin
		if Fields.Count < 2 then
			RowCount := 2
		else
			RowCount := Fields.Count +1;
		FixedRows := 1;

		if Fields.Count = 0 then
		begin
			Cells[0,1] := '';
			Cells[1,1] := '';
			Cells[2,1] := '';
			Cells[3,1] := '';
			Cells[4,1] := '';
			Cells[5,1] := '';
			Cells[6,1] := '';
			Exit; //===>>
		end;

		for i := 0 to Fields.Count -1 do
		begin
			Cells[0,i+1] := Fields[i].FieldName;
			ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
			System.Delete(ft, 1, 4);
			Cells[1,i+1] := ft;
			sVis := GetEnumName(TypeInfo(TbsVisibility), Ord(Fields[i].Visibility));
			System.Delete(sVis, 1, 2);
			Cells[2,i+1] := sVis;
			Cells[3,i+1] := BoolToStr(Fields[i].UseGetter, True);
			Cells[4,i+1] := BoolToStr(Fields[i].UseSetter, True);
			Cells[5,i+1] := BoolToStr(Fields[i].IsOverride, True);
			Cells[6,i+1] := BoolToStr(Fields[i].IsReintroduce, True);
		end;
	end;
end;

procedure TForm1.edtFieldNameChange(Sender: TObject);
begin
	btnAddField.Enabled := edtFieldName.Text <> '';
end;

procedure TForm1.btnRemoveClick(Sender: TObject);
begin
	with stgFields do
	begin
		if Row > 0 then
		begin
			Fields.Delete(Row -1);
			RefreshFieldList;
		end;
	end;
end;

procedure TForm1.btnRemoveAllClick(Sender: TObject);
begin
  Fields.List.Clear;
	RefreshFieldList;
end;

procedure TForm1.edtClassNameChange(Sender: TObject);
begin
	if chkUseClassName.Checked then
		edtTableName.Text := edtClassName.Text;
end;

procedure TForm1.chkUseClassNameClick(Sender: TObject);
begin
	edtTableName.Enabled := not chkUseClassName.Checked;
	if chkUseClassName.Checked then
		edtTableName.Text := edtClassName.Text
  else
	begin
		edtTableName.SetFocus;
		edtTableName.SelectAll;
	end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
	if (Fields.Count = 0) and (PageControl1.ActivePageIndex = 1) then
	begin
		edtFieldName.SetFocus;
		edtFieldName.SelectAll;
	end;
end;

procedure TForm1.stgFieldsClick(Sender: TObject);
begin
	if Fields.Count = 0 then Exit;
	if stgFields.Row < 1 then Exit;

	with stgFields do
	begin
		edtFieldName.Text       := Fields[Row-1].FieldName;
		rgrVisibility.ItemIndex := Ord(Fields[Row-1].Visibility);
		chkUseGetter.Checked    := Fields[Row-1].UseGetter;
		chkUseSetter.Checked    := Fields[Row-1].UseSetter;
		chkOverride.Checked     := Fields[Row-1].IsOverride;
		chkReintroduce.Checked  := Fields[Row-1].IsReintroduce;
		cmbFieldType.ItemIndex  := Ord(TtiTypeKind(Fields[Row-1].FieldType));
		btnRemove.Enabled       := Row > 0;
		btnRemoveAll.Enabled    := Row > 0;
	end;
end;

procedure TForm1.edtFieldNameKeyPress(Sender: TObject; var Key: Char);
begin
	case Key of
		'A'..'Z','a'..'z','0'..'9',#8,#9,'_': ;
		#27:  TEdit(Sender).Perform(WM_UNDO, 0, 0);
		else Key := #0;
	end;
end;

procedure TForm1.btnExportCSVClick(Sender: TObject);
var
	CN,sVis,ft: String;
	sl: TStringList;
	i: Integer;
begin

	if Fields.Count = 0 then
	begin
		ShowMessage('No Fields to Export!');
		Exit;
	end;

	CN := edtClassName.Text;
	sl := TStringList.Create;

	sl.Add('; ' + CN + ' Field Roster' +CL);

	{
		Notes:
		Only lines starting with A-Z or a-z are parsed, any other starting char skips the line.
		Definitions MUST be in this format, with NO extra commas or characters:
			fieldName,fieldType,visibility,useGetter,useSetter,isOVerride,isReintroduce
		Example:
			Last_Name,String,Published,True,False,False,False
	}

	try
		for i := 0 to Fields.Count -1 do
		begin
			sVis := GetEnumName(TypeInfo(TbsVisibility), Ord(Fields[i].Visibility));
			System.Delete(sVis, 1, 2);

			ft := GetEnumName(TypeInfo(TtiTypeKind), Ord(Fields[i].FieldType));
			System.Delete(ft, 1, 4);

			sl.Add( Fields[i].FieldName                      +','+
							ft                                       +','+
							sVis                                     +','+
							BoolToStr(Fields[i].UseGetter, True)     +','+
							BoolToStr(Fields[i].UseSetter, True)     +','+
							BoolToStr(Fields[i].IsOverride, True)    +','+
							BoolToStr(Fields[i].IsReintroduce, True) );
		end;

		sl.Add('' +CL+ '; Field Count: ' + IntToStr(Fields.Count));
		sl.SaveToFile(FAppDir + CN + '.csv');
		ShowMessage('Saved ' + FAppDir + CN + '.csv');

	finally
		sl.Free;
	end;
end;

procedure TForm1.btnImportCSVClick(Sender: TObject);
var
	CN, s: String;
	sName, sType, sVis, sGetter, sSetter, sOvr, sRei: String;
	sl: TStringList;
	iCurrentFieldCount,cc,i,j,p: Integer;
	od: TOpenDialog;
begin

	if Fields.Count > 0 then
		if MessageDlg('You already have fields defined.' +#13#10+ 'Add import list to them?', mtWarning, mbOKCancel, 0) <> IDOK then
			Exit;

	CN := edtClassName.Text;
	if Length(CN) < 1 then
	begin
		ShowMessage('Please enter a ClassName before Importing fields.');
		Exit;
	end;

	iCurrentFieldCount := Fields.Count;

	sl := TStringList.Create;

	od := TOpenDialog.Create(nil);
	od.Filter := 'CSV Files (*.csv)|*.csv';
	od.FilterIndex := 0;

	try

		// test open file
		if od.Execute then
		begin
			sl.LoadFromFile(od.FileName);
			if sl.Count = 0 then
			begin
				ShowMessage('That file is empty.');
				Exit;
			end else
			begin

				// check for correct number of args in each line
				for i := 0 to sl.Count -1 do
				begin
					cc := 0;

					if Length(sl[i]) < 10 then //can't be a valid line
						Continue;

					if not (sl[i][1] in ['A'..'Z','a'..'z']) then
						Continue; //skip blanks, comments
						
					for j := 1 to Length(sl[i]) do
						if sl[i][j] = ',' then
							Inc(cc);

					if cc <> 6 then
					begin
						ShowMessage('Incorrect number of arguments at line ' + IntToStr(i));
						Exit;
					end;
				end;
			end;
		end;

		// parse each line in fields list
		for i := 0 to sl.Count -1 do
		begin
			s := sl[i];

			// lines starting with non-A-Z are skipped
			if Length(s) < 1 then //blank line
				Continue;

			if not (s[1] in ['A'..'Z','a'..'z']) then
				Continue;

			// field name
			p := Pos(',', s);
			if p > 0 then
			begin
				sName := Trim(LeftStr(s, p-1));
				Delete(s, 1, p);
			end;

			// field type
			p := Pos(',', s);
			if p > 0 then
			begin
				sType := 'tiTK' + Trim(LeftStr(s, p-1));
				Delete(s, 1, p);
			end;

			// visibility
			p := Pos(',', s);
			if p > 0 then
			begin
				sVis := 'bs' + Trim(LeftStr(s, p-1));
				Delete(s, 1, p);
			end;

			// IsGetter
			p := Pos(',', s);
			if p > 0 then
			begin
				sGetter := Trim(LeftStr(s, p-1));
				Delete(s, 1, p);
			end;

			// IsSetter
			p := Pos(',', s);
			if p > 0 then
			begin
				sSetter := Trim(LeftStr(s, p-1));
				Delete(s, 1, p);
			end;

			// IsOverride
			p := Pos(',', s);
			if p > 0 then
			begin
				sOvr := Trim(LeftStr(s, p-1));
				Delete(s, 1, p);
			end;

			// IsReintroduce
			// this is the last field, so no more commas(!)
			if Length(s) > 1 then
			begin
				sRei := Trim(s);
			end;

			// check that the line was parsed OK
			if (sName = '') or (sType = '') or (sVis = '') or
				 (sOvr = '') or (sRei = '') or (sGetter = '') or (sSetter = '') then
			begin
				ShowMessage(Format('Bad Field Definition format at line %d.', [IntToStr(i+1)]));
				Exit;
			end;

			with Fields.Add do
			begin
				FieldName      := sName;
				FieldType      := TtiTypeKind(GetEnumValue(TypeInfo(TtiTypeKind), sType));
				Visibility     := TbsVisibility(GetEnumValue(TypeInfo(TbsVisibility), sVis));
				IsOverride     := StrToBool(sOvr);
				IsReintroduce  := StrToBool(sRei);
				UseGetter      := StrToBool(sGetter);
				UseSetter      := StrToBool(sSetter);
			end;

		end; //for i := 0 to sl.Count -1 do

		if Fields.Count = iCurrentFieldCount then
			ShowMessage('Nothing to do.')
		else
		begin
			RefreshFieldList;
			ShowMessage('Imported ' + IntToStr(Fields.Count - iCurrentFieldCount) + ' fields Ok.');
		end;

	finally
		sl.Free;
		od.Free;
	end;
end;

end.
