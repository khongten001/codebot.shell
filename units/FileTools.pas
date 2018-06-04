
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit FileTools;

interface

{$I CODEBOT.INC}

uses
  Windows, SysUtils, Classes;

{ GetTargetFileName }

function GetTargetFileName: string;

{ GetLocalFileName }

function GetLocalFileName(const FileName: string): string;

{ GetTempPath }

function GetTempPath: string;

{ GetTempFileName function }

function GetTempFileName(const Path: string = ''): string;

function FileTempName(const Path: string = ''): string;

{ ChangeFileName function }

function ChangeFileName(const FilePath: string; FileName: string): string;

{ GetShortFileName function }

function GetShortFileName(const FileName: string): string;

{ GetSystemPath function }

function GetSystemPath: string;

{ GetWindowsPath function }

function GetWindowsPath: string;

{ GetConsolePath function }

function GetConsolePath: string;

implementation

uses
  StrConst;

function GetTargetFileName: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsLibrary then
  begin
    GetModuleFileName(HInstance, Buffer, SizeOf(Buffer));
    Result := Buffer;
  end
  else
    Result := ParamStr(0);
end;

function GetLocalFileName(const FileName: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result);
  Result := Result + FileName;
end;

function GetTempPath: string;
begin
  SetLength(Result, MAX_PATH);
  Windows.GetTempPath(MAX_PATH, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetTempFileName(const Path: string = ''): string;
var
  TempPath: string;
begin
  if Path = '' then
    TempPath := GetTempPath
  else
    TempPath := IncludeTrailingPathDelimiter(Path);
  SetLength(Result, MAX_PATH);
  Windows.GetTempFileName(PChar(TempPath), '~TM', 0, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  DeleteFile(Result);
end;

function FileTempName(const Path: string = ''): string;
begin
  Result := GetTempFileName;
end;

function ChangeFileName(const FilePath: string; FileName: string): string;
var
  Path: string;
  S: string;
begin
  Path := FilePath;
  S := ExtractFileName(Path);
  SetLength(Path, Length(Path) - Length(S));
  Result := Path + FileName;
end;

function GetShortFileName(const FileName: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if GetShortPathName(PChar(FileName), Buffer, MAX_PATH) > 0 then
    Result := Buffer
  else
    RaiseLastWin32Error;
end;

function GetSystemPath: string;
begin
  SetLength(Result, MAX_PATH);
  GetSystemDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetWindowsPath: string;
begin
  SetLength(Result, MAX_PATH);
  GetWindowsDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetConsolePath: string;
const
  Consoles: array[Boolean] of string = ('command.com', 'cmd.exe');
var
  Info: TOSVersionInfo;
  S: string;
begin
  Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Info);
  S := Consoles[Info.dwPlatformId = VER_PLATFORM_WIN32_NT];
  if FileExists(GetSystemPath + S) then
    Result := GetSystemPath + S
  else if FileExists(GetWindowsPath + S) then
    Result := GetWindowsPath + S
  else
    Result := '';
end;

procedure GetFileList(const Directory: string; Strings: TStrings;
  Wildcards: string = '*.*');
var
  PriorSorted: Boolean;
  SearchRec: TSearchRec;
  SearchResult: Integer;
  Cards: TStrings;
  S: string;
  I: Integer;
begin
  Strings.Clear;
  S := Directory;
  if S = '' then Exit;
  S := IncludeTrailingPathDelimiter(S);
  Cards := TStringList.Create;
  try
    Cards.Text := StringReplace(WildCards, ';', #13#10, [rfReplaceAll]);
    for I := 0 to Cards.Count - 1 do
    begin
      SearchResult := FindFirst(S + Cards[I], faAnyFile and
        (not faDirectory), SearchRec);
      while SearchResult = 0 do
      begin
        Strings.AddObject(S + SearchRec.Name, TObject(SearchRec.Size));
        SearchResult := FindNext(SearchRec);
      end;
      FindClose(SearchRec);
    end;
  finally
    Cards.Free;
  end;
  if Strings is TStringList then
    with Strings as TStringList do
    begin
      PriorSorted := Sorted;
      Sorted := True;
      Sorted := PriorSorted;
    end;
end;

function GetFileSize(const FileName: string): Cardinal;
var
  SearchRec: TSearchRec;
  SearchResult: Integer;
begin
  SearchResult := FindFirst(FileName, faAnyFile and (not faDirectory), SearchRec);
  if SearchResult = 0 then
    Result := SearchRec.Size
  else
    Result := 0;
  FindClose(SearchRec);
end;

function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
end;

end.
