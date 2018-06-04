
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit WinTools;

interface

{$I CODEBOT.INC}

uses
  Classes, Windows, Messages, ActiveX, SysUtils, ShellAPI,
  FileTools;

function NumCpuCores: Integer;

{ The GetWindowClassName function }

function GetWindowClassName(Wnd: HWND): string;

{ The GetWindowCaption function }

function GetWindowCaption(Wnd: HWND): string;

{ The IsWindowClass function }

function IsWindowClass(const ClassName: string; const Module: string = ''): Boolean;

{ The GetDesktopWindows procedure }

type
  TWindowStringFormat = set of (sfCaption, sfClassName, sfHandle, sfVisibility);

procedure GetDesktopWindows(Windows: TStrings; Format: TWindowStringFormat);

{ The GetChildWindows procedure }

procedure GetChildWindows(Parent: HWND; Windows: TStrings; Format: TWindowStringFormat);

{ The HideTaskbarIcon procedure }

procedure HideTaskbarIcon(Wnd: HWND);

{ The window position routines are used to query and modify the dimensions of
  a window using the TWindowPosition structure }

type
  TWindowPosition = record
    case Boolean of
     False: (
      Left: Integer;
      Top: Integer;
      Width: Integer;
      Height: Integer);
    True: (
      Pos: TPoint;
      Size: TPoint);
  end;

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);
function IsWindowPosition(Wnd: HWND; const Pos: TWindowPosition): Boolean;

{ Bounds as related to the taskbar }

function GetTaskbarRect: TRect;
function GetTrayBounds(Width, Height: Integer; Offset: Integer = 10): TRect;

{ Global shortcut reoutines }

function RegisterShortCut(Wnd: HWND; Id: Integer; ShortCut: TShortCut): Boolean;
function UnregisterShortCut(Wnd: HWND; Id: Integer): Boolean;

{ The InvalidateWidows procedure }

procedure InvalidateWindows(Wnd: HWND);

{ The GetEnvironmentVariable function }

function GetEnvironmentVariable(const Name: string): string;

{ The TerminateProcess function }

function TerminateProcess(Process: THandle): Boolean;

{ The IsProcessWindow function }

function IsProcessWindow(Wnd: HWND): Boolean;

{ The IsProcess function }

function IsProcess(Process: THandle): Boolean;

{ The WindowFromPoint function returns any window, child or not, below the
  Point parameter. This function ignores hidden windows }

function WindowFromPoint(const Point: TPoint): HWND;

{ The GetDialogParent function }

function GetDialogParent(Wnd: HWND): HWND;

{ The ShutdownWindows procedure exits the current window session }

procedure ShutdownWindows;

function IsWindowsXPOrLater: Boolean;
type
  TScanProc = procedure(Memory: Pointer; Size: Integer);

procedure ScanProcessMemory(ScanProc: TScanProc);

{ TPerformanceTimer class }

type
  EPerformanceError = class(Exception);

  TPerformanceTimer = class(TObject)
  private
    FResolution: Int64;
    FStart: Int64;
    FStop: Int64;
    FTiming: Boolean;
    function GetElapsedTime: LongWord;
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    property ElapsedTime: LongWord read GetElapsedTime;
  end;

implementation

uses
  StrConst;

function NumCpuCores: Integer;
asm
  PUSH  EBX
  MOV    EAX, 1
  CPUID
  MOV    EAX, EBX
  AND    EAX, $FF0000
  SHR    EAX, $000010
  POP   EBX
  TEST  EAX, EAX
  JNZ   @done
  MOV    EAX, 1
  @done:
end;

function GetWindowClassName(Wnd: HWND): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsWindow(Wnd) and (GetClassName(Wnd, Buffer, MAX_PATH) <> 0) then
    Result := Buffer
  else
    Result := '';
end;

function GetWindowCaption(Wnd: HWND): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsWindow(Wnd) and (GetWindowText(Wnd, Buffer, MAX_PATH) <> 0) then
    Result := Buffer
  else
    Result := '';
end;

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
begin
  GetWindowRect(Wnd, TRect(Pos));
  with Pos do
  begin
    Dec(Width, Left);
    Dec(Height, Top);
  end;
  MapWindowPoints(GetDesktopWindow, GetParent(Wnd), TRect(Pos).TopLeft, 1);
end;

procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);
begin
  with Pos do
    MoveWindow(Wnd, Left, Top, Width, Height, True);
end;

var
  WindowsStringFormat: TWindowStringFormat;

function EnumWindowsCallback(Wnd: HWND; Windows: TStrings): BOOL; stdcall;
const
  WindowStatus: array[Boolean] of string = ('hidden', 'visible');
var
  S: string;
begin
  S := '';
  if sfCaption in WindowsStringFormat then
    S := '"' + GetWindowCaption(Wnd) + '"';
  if sfClassName in WindowsStringFormat then
    S := Trim(S + ' ' + GetWindowClassName(Wnd));
  if sfHandle in WindowsStringFormat then
    S := Trim(S + ' [' + IntToHex(Wnd, 8) + ']');
  if sfVisibility in WindowsStringFormat then
    S := Trim(S + ' (' + WindowStatus[IsWindowVisible(Wnd)] + ')');
  if  WindowsStringFormat = [sfCaption] then
    S := Copy(S, 2, Length(S) - 2);
  Windows.AddObject(S, TObject(Wnd));
  Result := True;
end;

procedure GetDesktopWindows(Windows: TStrings; Format: TWindowStringFormat);
begin
  Windows.BeginUpdate;
  try
    Windows.Clear;
    WindowsStringFormat := Format;
    EnumWindows(@EnumWindowsCallback, Integer(Windows));
  finally
    Windows.EndUpdate;
  end;
end;

procedure GetChildWindows(Parent: HWND; Windows: TStrings; Format: TWindowStringFormat);
begin
  Windows.BeginUpdate;
  try
    Windows.Clear;
    WindowsStringFormat := Format;
    EnumChildWindows(Parent, @EnumWindowsCallback, Integer(Windows));
  finally
    Windows.EndUpdate;
  end;
end;

procedure HideTaskbarIcon(Wnd: HWND);
begin
  SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE
    and not WS_EX_APPWINDOW) or WS_EX_TOOLWINDOW);
  ShowWindow(Wnd, SW_HIDE);
end;

function IsWindowClass(const ClassName: string;
  const Module: string = ''): Boolean;
var
  WndClass: TWndClass;
  Handle: HMODULE;
begin
  FillChar(WndClass, SizeOf(WndClass), #0);
  if Module <> '' then
  begin
    Handle := GetModuleHandle(PChar(Module));
    if Handle <> 0 then
      Result := GetClassInfo(Handle, PChar(ClassName), WndClass)
    else
      Result := False;
  end
  else
    Result :=  GetClassInfo(HInstance, PChar(ClassName), WndClass);
end;

function GetEnvironmentVariable(const Name: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if Windows.GetEnvironmentVariable(PChar(Name), Buffer, MAX_PATH) > 0 then
    Result := Buffer
  else
    Result := '';
end;

function TerminateProcess(Process: THandle): Boolean;
begin
  Process := OpenProcess(PROCESS_ALL_ACCESS, TRUE, Process);
  if (Process <> 0) then
  begin
   Result := Windows.TerminateProcess(Process, 0);
   CloseHandle(Process);
  end
  else
    Result := False;
end;

function IsProcessWindow(Wnd: HWND): Boolean;
var
  Process: THandle;
begin
  Result := IsWindow(Wnd);
  if Result then
  begin
    GetWindowThreadProcessId(Wnd, @Process);
    Result := Process = GetCurrentProcessID;
  end;
end;

function IsProcess(Process: THandle): Boolean;
var
  ExitCode: DWORD;
begin
  Result := GetExitCodeProcess(Process, ExitCode) and (ExitCode = STILL_ACTIVE);
end;

function IsWindowPosition(Wnd: HWND; const Pos: TWindowPosition): Boolean;
var
  CurrentPos: TWindowPosition;
begin
  GetWindowPosition(Wnd, CurrentPos);
  Result := CompareMem(@CurrentPos, @Pos, SizeOf(TWindowPosition));
end;

function GetTaskbarRect: TRect;
var
  W: HWND;
begin
  W := FindWindow('Shell_TrayWnd', nil);
  if W <> 0 then
    GetWindowRect(W, Result)
  else
    FillChar(Result, SizeOf(Result), #0);
end;

function GetTrayBounds(Width, Height: Integer; Offset: Integer = 10): TRect;
var
  R: TRect;
  SW, SH: Integer;
begin
  R := GetTaskbarRect;
  SW := GetSystemMetrics(SM_CXSCREEN);
  SH := GetSystemMetrics(SM_CYSCREEN);
  if (R.Left < 1) and (R.Top < 1) then
    if R.Bottom < SH then
      Result.TopLeft := Point(SW - Width - Offset, R.Bottom + Offset)
    else
      Result.TopLeft := Point(R.Right + Offset, SH - Height - Offset)
  else if R.Left < 1 then
    Result.TopLeft := Point(SW - Width - Offset, R.Top - Height - Offset)
  else
    Result.TopLeft := Point(R.Left - Width - Offset, SH - Height - Offset);
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + Height;
end;

function RegisterShortCut(Wnd: HWND; Id: Integer; ShortCut: TShortCut): Boolean;
var
  M, K: Cardinal;
begin
  M := 0;
  if ShortCut and scShift = scShift then
    M := M or MOD_SHIFT;
  if ShortCut and scCtrl = scCtrl then
    M := M or MOD_CONTROL;
  if ShortCut and scAlt = scAlt then
    M := M or MOD_ALT;
  K := ShortCut and $FFF;
  Result := RegisterHotKey(Wnd, Id, M, K);
end;

function UnregisterShortCut(Wnd: HWND; Id: Integer): Boolean;
begin
  Result := UnregisterHotKey(Wnd, Id);
end;

function InvalidateCallback(Wnd: HWND; Unused: Integer): BOOL; stdcall;
begin
  InvalidateRect(Wnd, nil, True);
  EnumChildWindows(Wnd, @InvalidateCallback, 0);
  Result := True;
end;

procedure InvalidateWindows(Wnd: HWND);
begin
  InvalidateRect(Wnd, nil, True);
  EnumChildWindows(Wnd, @InvalidateCallback, 0);
end;

function WindowFromPoint(const Point: TPoint): HWND;
var
  Wnd: HWND;
  P: TPoint;
begin
  Result := 0;
  Wnd := GetDesktopWindow;
  while (Wnd <> Result) and IsWindow(Wnd) do
  begin
    Result := Wnd;
    P := Point;
    ScreenToClient(Result, P);
    Wnd := ChildWindowFromPointEx(Result, P, CWP_SKIPINVISIBLE);
  end;
end;

function GetDialogParent(Wnd: HWND): HWND;
begin
  Result := Wnd;
  if IsWindow(Result) then
    while GetWindowLong(Result, GWL_STYLE) and WS_CHILD = WS_CHILD do
      Result := GetParent(Result)
  else
    Result := 0;
end;

function CreateProcessAndReturn(const AppName: string; ShowState: Integer): THandle;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := ShowState;
  end;
  if CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
    Result := ProcessInfo.dwProcessId
  else
    Result := 0;
end;

procedure CreateProcessAndWait(const AppName: string; ShowState: Integer); overload;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := ShowState;
  end;
  if CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
  begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;

procedure CreateDesktopTask(const AppName, Desktop: string);
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    StartupInfo.lpDesktop := Pointer(Desktop);
  end;
  if CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
  begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;

function IsWindowsXPOrLater: Boolean;
var
  V: DWORD;
begin
  V := GetVersion;
  if LongRec(V).Lo < 5 then
    Result := False
  else if LongRec(V).Lo > 5 then
    Result := True
  else
    Result := LongRec(V).Hi > 0;
end;

procedure ScanProcessMemory(ScanProc: TScanProc);
var
  SystemInfo: TSystemInfo;
  Process: THandle;
  Memory: Cardinal;
  MemoryInformation: TMemoryBasicInformation;
begin
  GetSystemInfo(SystemInfo);
  Process := GetCurrentProcess;
  Memory := 0;
  while Memory < Cardinal(SystemInfo.lpMaximumApplicationAddress) do
  begin
    MemoryInformation.RegionSize := 0;
    VirtualQueryEx(Process, Pointer(Memory), MemoryInformation,
      SizeOf(TMemoryBasicInformation));
    with MemoryInformation do
    begin
      Memory := Cardinal(BaseAddress) + Cardinal(RegionSize);
      if (AllocationProtect and PAGE_READWRITE = PAGE_READWRITE) and
        (Type_9 = MEM_PRIVATE) and (State = MEM_COMMIT) then
        ScanProc(BaseAddress, RegionSize);
    end;
  end;
end;

procedure ShutdownWindows;
var
  ProcessHandle: Integer;
  TokenHandle: THandle;
  TokenPrivileges: TTokenPrivileges;
  Dummy: TTokenPrivileges;
  Buffer: DWORD;
begin
  with TokenPrivileges, Privileges[0] do
  begin
    PrivilegeCount := 1;
    LookupPrivilegeValue(nil, 'SeShutdownPrivilege', LUID);
    Attributes := SE_PRIVILEGE_ENABLED;
  end;
  ProcessHandle := GetCurrentProcess;
  OpenProcessToken(ProcessHandle, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, TokenHandle);
  AdjustTokenPrivileges(TokenHandle, False, TokenPrivileges, SizeOf(TokenPrivileges),
    Dummy, Buffer);
  ExitWindowsEx(EWX_SHUTDOWN or EWX_FORCE or EWX_REBOOT, 0);
end;

{ TPerformanceTimer }

constructor TPerformanceTimer.Create;
begin
  if not QueryPerformanceFrequency(FResolution) then
    RaiseLastWin32Error;
  {$IFDEF D5_UP}
     FResolution := FResolution div 1000;
  {$ELSE}
     FResolution := FResolution / 1000;
  {$ENDIF}
end;

function TPerformanceTimer.GetElapsedTime: LongWord;
var
  EndTime: Int64;
begin
  if FTiming then
  begin
    if not QueryPerformanceCounter(EndTime) then
      raise EPerformanceError.CreateFmt(STimerError, ['query']);
  end
  else
    EndTime := FStop;
  Result := (EndTime - FStart) div FResolution;
end;

procedure TPerformanceTimer.Start;
begin
  if not QueryPerformanceCounter(FStart) then
    raise EPerformanceError.CreateFmt(STimerError, ['start']);
  FTiming := True;
end;

procedure TPerformanceTimer.Stop;
begin
  if not QueryPerformanceCounter(FStop) then
    raise EPerformanceError.CreateFmt(STimerError, ['stop']);
  FTiming := False;
end;

end.
