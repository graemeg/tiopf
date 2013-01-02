{ THelpContextMap

  Storage container for help context numbers.

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

unit ehscontextmap;

interface

{$I ESHHelpWarn.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
   PContextPair = ^TContextPair;
   TContextPair = record
     HashVal: longint;
     CntxVal: THelpContext;
   end;

  THelpContextMap = class(TComponent)
  private
    fFileName: TFileName;
    fData: TMemoryStream;
    procedure SetFileName(value: TFilename);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadFileData(Stream: TStream); virtual;
    procedure WriteFileData(Stream: TStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetContext(TopicID: string): THelpContext;
  published
    property FileName: TFileName read fFileName write SetFileName;
  end;

implementation

uses ehshshtb;


constructor THelpContextMap.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     fData := TMemoryStream.create;
end;

destructor THelpContextMap.Destroy;
begin
     fData.free;
     inherited Destroy;
end;

procedure THelpContextMap.DefineProperties(Filer: TFiler);
begin
     inherited DefineProperties(Filer);
     Filer.DefineBinaryProperty('FileData', ReadFileData, WriteFileData, (fData.size > 0));
end;

procedure THelpContextMap.ReadFileData(Stream: TStream);
var
   Size: longint;
begin
     fData.clear;
     Stream.ReadBuffer(Size, SizeOf(Size));
     fData.CopyFrom(Stream, Size);
end;

procedure THelpContextMap.WriteFileData(Stream: TStream);
var
   Size: longint;
begin
     fData.seek(0,0);
     Size := fData.size;
     Stream.WriteBuffer(Size, SizeOf(Size));
     Stream.CopyFrom(fData, Size);
end;

procedure THelpContextMap.SetFileName(value: TFilename);
var
   CList: TList;
   L: integer;

   procedure ParseFile(const fname: string; IsIncludeFile: boolean);
   var
     IsMap: boolean;
     f: Textfile;
     s: string;
     h, c: string;
     i, hi, ci: integer;
     hval: boolean;
     P: PContextPair;
     LastDir, ExpandedName: string;
   begin
     assignFile(f, fname);
     reset(f);
     try
       IsMap := false;
       while not eof(f) do
       begin
          readln(f, s);
          s := trim(s);
          if (IsMap or IsIncludeFile) and (s <> '') then
          begin
             if strLIcomp('[',pchar(s),1) = 0 then exit;  //break

             if strLIcomp('#include',pchar(s),8) = 0 then  //parse include file
             begin
                  LastDir := GetCurrentDir;
                  SetCurrentDir(ExtractFileDir(fname));
                  ExpandedName := ExpandFileName(trim(copy(s, 9, length(s)-8)));
                  if FileExists(ExpandedName)
                    then ParseFile(ExpandedName, true)
                    else MessageDlg('Include file "'+ExpandedName+'" not found!', mtError, [mbOK], 0);
                  SetCurrentDir(LastDir);
             end
             else begin
               if strLIcomp('#define',pchar(s),7) = 0 then s := copy(s,9,length(s)-8);
               hval := true;
               h := '';
               c := '';
               for i := 1 to length(s) do
               begin
                    case s[i] of
                    ';','#': break;
                    '=':     hval := false;
                    #9,#32:  if h <> '' then hval := false;
                    else
                      begin
                         if hval then h := h + s[i]
                                 else c := c + s[i];
                      end;
                    end;
               end;
               if (h <> '') and (c <> '') then
               try
                  ci := strtoint(c);
                  hi := GetHashValue(h);
                  if hi <> 0 then
                  begin
                       New(P);
                       P^.hashval := hi;
                       P^.cntxval := ci;
                       Clist.add(P);
                  end;
               except;
               end;
             end;
          end  //IsMap
          else IsMap := strLIcomp('[MAP]',pchar(s),5) = 0;
       end;
     finally
       closefile(f);
     end;
   end;

   function CBCListSort(Item1, Item2: Pointer): Integer;
   begin
        result := 0;
        if PContextPair(Item1)^.HashVal < PContextPair(Item2)^.HashVal then result := -1
        else if PContextPair(Item1)^.HashVal > PContextPair(Item2)^.HashVal then result := 1;
   end;

begin
     if (value <> '')
       and (not (csLoading in ComponentState))
       and (not (csReading in ComponentState)) then
     try
        if (value <> '') and (not FileExists(value)) then
        begin
          raise Exception.create('File '+value+' not found.');
          fData.clear;
          exit;
        end;


        fData.clear;
        CList := TList.create;
        ParseFile(value, false);
        CList.sort(TListSortCompare(@CBCListSort));
        for L := 0 to CList.count-1 do
        begin
             fData.WriteBuffer(PContextPair(CList[L])^.HashVal, sizeof(longint));
             fData.WriteBuffer(PContextPair(CList[L])^.CntxVal, sizeof(THelpContext));
        end;
        if (csDesigning in ComponentState)
          then MessageDlg('Help context list updated. '
                          +inttostr(CList.count)
                          +' items stored.',
                          mtInformation, [mbOK], 0);
     finally
        for L := 0 to CList.count-1 do Dispose(PContextPair(CList[L]));
        CList.free;
     end;
     fFilename := value;
end;

function THelpContextMap.GetContext(TopicID: string): THelpContext;
var
   rclen: integer;
   L, R, M: integer;
   Hi, HashVal: longint;
begin
     result := 0;
     HashVal := GetHashValue(TopicID);
     if (HashVal <> 0) and (fData <> nil) and (fData.size > 0) then
     begin
          rclen := sizeof(longint) + sizeof(THelpContext);

          L := 0;
          R := pred(fData.size div rclen);
          while L <= R do
          begin
               M := (L + R) div 2;
               fData.seek(M * rclen, soFromBeginning);

               fData.readbuffer(Hi, sizeof(Hi));
               if Hi = Hashval then
               begin
                    fData.readbuffer(result, sizeof(result));
                    exit;
               end
               else
                 if (Hi < HashVal) then L := M + 1
                                   else R := M - 1;
          end;
     end;
end;

end.
