{
  Copyright 2006-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Logging. Log has to be activated in your program (nothing in the
  Castle Game Engine activates it automatically) by InitializeLog. }
unit CastleLog;

{$include castleconf.inc}

interface

uses Classes;

{ Is logging active. Initially no. Activate by InitializeLog.

  @deprecated
  Instead of checking "if Log then WritelnLog(...)",
  *always* call WriteLnLog / WriteLnWarning,
  this allows to collect logs even before InitializeLog may be called.
  Even release applications usually have logging = on,
  so it makes no sense to optimize for the "logging = off" case,
  you need to make "logging = on" case always fast.

  @exclude
}
function Log: boolean;
  deprecated 'do not check is Log initialized';

type
  { Prefix each log line with optional date/time. }
  TLogTimePrefix = (
    { No prefix. }
    ltNone,
    { Record a time for each log record. }
    ltTime,
    { Record date and time for each log record. }
    ltDateTime
  );

{ Initialize logging.
  See https://castle-engine.io/manual_log.php for more documentation about logging.

  Where do we write the log:

  @unorderedList(
    @item(To the ALogStream, if you provide this parameter and it has non-nil value.)

    @item(Otherwise, to the LogFileName, if you set this global variable to non-empty value.)

    @item(Otherwise, we detect the best place to store the log automatically.
      This detection depends on various factors -- the operating system,
      whether the application is GUI/console (matters for Windows),
      whether we are inside a shared library etc.
      The exact algorithm is described on https://castle-engine.io/manual_log.php .

      You can always check @link(LogOutput) value to see where the output is going now.
      You can e.g. display @link(LogOutput) somewhere in UI.
    )
  )

  In case of some platforms (Android and Nintendo Switch, now)
  the log also @italic(always) goes to the device-specific log facility.
  In case of Android, this is just "adb logcat", visible also if you run
  "castle-engine run --target=android".
  This is done regardless of the ALogStream or LogFileName values.

  @param(ALogTimePrefix optionally adds date&time prefix to each log record.)
}
procedure InitializeLog(
  const ALogStream: TStream = nil;
  const ALogTimePrefix: TLogTimePrefix = ltNone); overload;

procedure InitializeLog(const ProgramVersion: string;
  const ALogStream: TStream = nil;
  const ALogTimePrefix: TLogTimePrefix = ltNone); overload;
  deprecated 'to provide a Version to InitializeLog, set ApplicationProperties.Version earlier, instead of calling InitializeLog with an explicit ProgramVersion parameter';

{ Log message. }
procedure WritelnLog(const Category: string; const Message: string); overload;
procedure WritelnLog(const Message: string); overload;

{ Format and log a message.
  This is a shortcut for @code(WritelnLog(Category, Format(MessageBase, Args))). }
procedure WritelnLog(const Category: string; const MessageBase: string;
  const Args: array of const); overload;
procedure WritelnLog(const MessageBase: string;
  const Args: array of const); overload;

{ Log message, without appending newline at the end (given Message
  should already contain a final newline). }
procedure WriteLog(const Category: string; const Message: string); overload;
  deprecated 'use WritelnLog, and do not add the final newline yourself to Message';

{ Log multiline message.
  The Message may, but doesn't have to, terminate with a newline --
  we will format it OK either way. }
procedure WritelnLogMultiline(const Category: string; const Message: string);

procedure WriteLogMultiline(const Category: string; const Message: string); deprecated 'use WritelnLogMultiline';

{ Log a warning, and call
  @link(TCastleApplicationProperties.OnWarning ApplicationProperties.OnWarning)
  event.

  This outputs a log message.
  We simply append the word "warning" to the Category, and pass arguments
  to WritelnLog.

  Then, @italic(regardless if the log is initialized or not),
  we also call @link(TCastleApplicationProperties.OnWarning ApplicationProperties.OnWarning).
  This allows to react to warnings e.g. by displaying a message dialog
  (like @code(ShowMessage) in Lazarus, or @link(MessageOK) in CastleMessages,
  or @link(TCastleWindowBase.MessageOK)).
  Or by raising an exception, if you want to be strict about warnings. }
procedure WritelnWarning(const Category: string; const Message: string); overload;
procedure WritelnWarning(const Message: string); overload;

{ A shortcut for @code(WritelnWarning(Category, Format(MessageBase, Args))). }
procedure WritelnWarning(const Category: string; const MessageBase: string;
  const Args: array of const); overload;
procedure WritelnWarning(const MessageBase: string;
  const Args: array of const); overload;

var
  { Dump backtrace (call stack) with each log.
    Displaying line info requires compiling your program with -gl.
    Note that displaying a backtrace may slow down logging considerably,
    so use this only when you really need it, and disable for the final build. }
  BacktraceOnLog: boolean = false;

  { Current log date/time prefix style. Can be changed at runtime. }
  LogTimePrefix: TLogTimePrefix;

  { Set this to a filename that should contain log,
    before calling @link(InitializeLog).
    This may be an absolute or relative (to the current directory
    at the time of InitializeLog call) path.
    Note that this variable doesn't support URLs. It is only a simple filename.

    It's your responsibility to choose a path that is writeable on current OS
    (you can e.g. use GetAppConfigDir function from FPC RTL). }
  LogFileName: String = '';

  { Enable logging to StdOut, which is used on some platforms (like on Unix) and situations
    (like when run under CGE editor or CGE build tool).
    If @false, we will always log to some file. }
  LogEnableStandardOutput: Boolean = true;

{$ifdef CASTLE_NINTENDO_SWITCH}
{ Send a log message using platform-specific logging on NX.

  You should always call WritelnLog instead of this,
  unless you're possibly before memory manager was initialized,
  or before unit's initialization took place.
}
procedure cgeNxLog(Message: PChar); cdecl; external 'cgeNxLog';
{$endif CASTLE_NINTENDO_SWITCH}

{ Where it the log output going. }
function LogOutput: String;

implementation

uses SysUtils,
  CastleUtils, CastleClassUtils, CastleTimeUtils,
  CastleStringUtils
  {$ifdef ANDROID}, CastleAndroidInternalLog {$endif};

{ On mobile platforms, do not place the log in GetAppConfigDir.
  GetAppConfigDir may be '' and creating a subdirectory there may fail.
  Although GetAppConfigDir is fixed in FPC trunk
  (see too http://wiki.freepascal.org/Android )
  but we still need to work with FPC 3.0.x.

  ApplicationConfig has better logic for this,
  using ApplicationConfigOverride, but it's set only from castlewindow_android.inc
  once the activity starts. }
{$define CASTLE_USE_GETAPPCONFIGDIR_FOR_LOG}
{$ifdef ANDROID} {$undef CASTLE_USE_GETAPPCONFIGDIR_FOR_LOG} {$endif}
{$ifdef CASTLE_IOS} {$undef CASTLE_USE_GETAPPCONFIGDIR_FOR_LOG} {$endif}

var
  FLog: boolean = false;
  FLogOutput: String;
  LogStream: TStream;
  LogStreamOwned: boolean;
  CollectedLog: String; //< log contents not saved to file yet

const
  { To avoid wasting time in applications that just never call InitializeLog,
    stop adding things to CollectedLog when it reaches certain length. }
  MaxCollectedLogLength = 80 * 100;

procedure WriteLogCoreCore(const S: string); forward;

function Log: boolean;
begin
  Result := FLog;
end;

procedure InitializeLog(const ProgramVersion: string;
  const ALogStream: TStream;
  const ALogTimePrefix: TLogTimePrefix);
begin
  InitializeLog(ALogStream, ALogTimePrefix);
end;

procedure InitializeLog(
  const ALogStream: TStream;
  const ALogTimePrefix: TLogTimePrefix);

  function InitializeLogFile(const LogFileName: string): boolean;
  begin
    try
      { without fmShareDenyNone, you cannot open the file while plugin runs }
      LogStream := TFileStream.Create(LogFileName, fmCreate or fmShareDenyNone);
      FLogOutput := LogFileName;
    except
      on E: EFCreateError do
      begin
        { Special message when LogFileName non-empty (usual case on Windows).
          Merely warn when creating log file not possible.
          Normal in many "production" cases when the directory of exe/plugin may not be writeable. }
        WritelnWarning('Log', 'Cannot create log file "' + LogFileName + '"');
        Exit(false);
      end;
    end;
    LogStreamOwned := true;
    Result := true;
  end;

  {$ifdef CASTLE_USE_GETAPPCONFIGDIR_FOR_LOG}
  { Similar to CastleFilesUtils.ApplicationConfig directory,
    but returns a filename, and doesn't depend on CastleFilesUtils and friends. }
  function ApplicationConfigPath: string;
  begin
    Result := InclPathDelim(GetAppConfigDir(false));
    if not ForceDirectories(Result) then
      raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
        [Result]);
  end;
  {$endif CASTLE_USE_GETAPPCONFIGDIR_FOR_LOG}

var
  WantsLogToStandardOutput: Boolean;
  EnableStandardOutput: Boolean;
begin
  LogTimePrefix := ALogTimePrefix;

  if FLog then Exit; { ignore 2nd call to InitializeLog }

  LogStreamOwned := false;
  FLogOutput := '';

  WantsLogToStandardOutput := GetEnvironmentVariable('CASTLE_LOG') = 'stdout';
  EnableStandardOutput := LogEnableStandardOutput and (not IsLibrary);

  if ALogStream <> nil then
  begin
    LogStream := ALogStream;
    FLogOutput := '<custom-stream>';
  end
  {$ifndef CASTLE_NINTENDO_SWITCH}
  else
  if WantsLogToStandardOutput and EnableStandardOutput and (StdOutStream <> nil) then
  begin
    { In this case, we know we have StdOutStream initialized OK,
      even when IsConsole = false (because "castle-engine run"
      and "castle-editor" are calling subprocesses to capture output
      through pipes always).

      We want to log to StdOutStream, to send them to "castle-editor"
      output list. }
    LogStream := StdOutStream;
    FLogOutput := '<stdout>';
  end else
  { If not in CGE editor, then LogFileName takes precedence over everything else. }
  if LogFileName <> '' then
  begin
    if not InitializeLogFile(LogFileName) then
      Exit;
  end else
  { In a library (like Windows DLL) create log file in user's config directory.
    Same thing for Windows GUI program.

    Note: We should not write to
      ChangeFileExt(ParamStr(0), '.log'))
    Although it seems most natural on Windows when debugging,
    it fails when the application is installed in a read-only directory under "Program Files" or such.

    Note: We should never write to StdOutStream when IsConsole=false.
    Although in some cases, GUI program may have an stdout
    (when it is explicitly run like "xxx.exe > xxx.log"),
    but we cannot rely on it. It's better to always write to file in case of IsConsole=false.
  }
  {$ifdef CASTLE_USE_GETAPPCONFIGDIR_FOR_LOG}
  if (not EnableStandardOutput) or (not IsConsole) then
  begin
    if not InitializeLogFile(ApplicationConfigPath + ApplicationName + '.log') then
      Exit;
  end else
  {$endif CASTLE_USE_GETAPPCONFIGDIR_FOR_LOG}
  begin
    LogStream := StdOutStream;
    FLogOutput := '<stdout>';
  end
  {$endif CASTLE_NINTENDO_SWITCH}
  ;

  { Note: on CASTLE_NINTENDO_SWITCH, it is possible to leave LogStream = nil.
    It doesn't matter, we will log to cgeNxLog anyway. }

  WriteLogCoreCore('Log for "' + ApplicationName + '".' + NL);
  WriteLogCoreCore('  Started on ' + DateTimeToAtStr(CastleNow) + '.' + NL);
  WriteLogCoreCore('  Castle Game Engine version: ' + CastleEngineVersion + '.' + NL);
  WriteLogCoreCore('  Compiled with ' + SCompilerDescription + '.' + NL);
  WriteLogCoreCore('  Platform: ' + SPlatformDescription + '.' + NL);
  if CollectedLog <> '' then
  begin
    WriteLogCoreCore(CollectedLog);
    CollectedLog := '';
  end;

  { In case of exception in WriteStr in WriteLogCoreCore,
    leave FLog as false.
    This way following WritelnLog will not again cause the same exception. }
  FLog := true;

  if WantsLogToStandardOutput and EnableStandardOutput and (StdOutStream = nil) then
    WritelnWarning('Cannot send logs through pipes. CASTLE_LOG=stdout defined but run as a GUI application. CASTLE_LOG=stdout should only be used internally, when running from the CGE editor or CGE build tool.');
end;

function LogOutput: String;
begin
  if not FLog then
    Result := '<logging-not-initialized>'
  else
    Result := FLogOutput;
end;

{ Add the String to log contents.
  Assumes that log is initialized.
  Sends it to AndroidLog and LogStream and ApplicationProperties._Log. }
procedure WriteLogCoreCore(const S: string);
begin
  // Assert(FLog); // do not check it, as InitializeLog uses it before FLog := true

  {$ifdef ANDROID}
  AndroidLogRobust(alInfo, S);
  {$endif}

  {$ifdef CASTLE_NINTENDO_SWITCH}
  cgeNxLog(PChar(S));
  {$else}
  // we know that LogStream <> nil when FLog = true
  WriteStr(LogStream, S);
  {$endif CASTLE_NINTENDO_SWITCH}
end;

{ Add the String to log contents.
  - Optionally adds backtrace to the String.
  - If log not initialized yet, adds the String to CollectedLog.
  - If log initialized, sends it to AndroidLog and LogStream and ApplicationProperties._Log
}
procedure WriteLogCore(const S: string);
var
  LogContents: String;
begin
  LogContents := S;
  {$ifdef FPC}
  if BacktraceOnLog then
    LogContents := LogContents + DumpStackToString(Get_Frame) + NL;
  {$endif}

  if FLog then
  begin
    WriteLogCoreCore(LogContents);
  end else
  if Length(CollectedLog) < MaxCollectedLogLength then
  begin
    CollectedLog := CollectedLog + LogContents;
    if Length(CollectedLog) >= MaxCollectedLogLength then
      CollectedLog := CollectedLog + '(... Further log messages will not be collected, until you call InitializeLog ...)' + NL;
  end;
end;

function LogTimePrefixStr: string;
begin
  case LogTimePrefix of
    ltNone: Result := '';
    ltTime: Result := FormatDateTime('tt', CastleNow) + '> ';
    ltDateTime: Result := FormatDateTime('yyyy"-"mm"-"dd" "tt', CastleNow) + '> ';
  end;
end;

procedure WriteLog(const Category: string; const Message: string);
var
  S: String;
begin
  S := LogTimePrefixStr;
  if Category <> '' then
    S := S + Category + ': ';
  S := S + Message;
  WriteLogCore(S);
end;

procedure WritelnLog(const Category: string; const Message: string);
begin
  // do not warn about using deprecated WriteLog here.
  // In the future, WriteLog should be moved to the "implementation" section
  // of the unit (internal), and undeprecated.
  {$warnings off}
  WriteLog(Category, Message + NL);
  {$warnings on}
end;

procedure WritelnLog(const Message: string);
begin
  WritelnLog('', Message);
end;

procedure WritelnLog(const Category: string; const MessageBase: string;
  const Args: array of const);
begin
  WritelnLog(Category, Format(MessageBase, Args));
end;

procedure WritelnLog(const MessageBase: string;
  const Args: array of const);
begin
  WritelnLog('', Format(MessageBase, Args));
end;

procedure WriteLogMultiline(const Category: string; const Message: string);
begin
  WritelnLogMultiline(Category, Message);
end;

procedure WritelnLogMultiline(const Category: string; const Message: string);
begin
  if LogTimePrefix <> ltNone then
    WriteLogCore(LogTimePrefixStr + NL);
  WriteLogCore(
    '-------------------- ' + Category + ' begin' + NL +
    // trim newlines at the end of Message
    TrimEndingNewline(Message) + NL +
    '-------------------- ' + Category + ' end' + NL)
end;

procedure WritelnWarning(const Category: string; const Message: string);
var
  WarningCategory: String;
begin
  if Category <> '' then
    WarningCategory := 'Warning: ' + Category
  else
    WarningCategory := 'Warning';
  WritelnLog(WarningCategory, Message);
end;

procedure WritelnWarning(const Message: string);
begin
  WritelnWarning('', Message);
end;

procedure WritelnWarning(const Category: string; const MessageBase: string;
  const Args: array of const);
begin
  WritelnWarning(Category, Format(MessageBase, Args));
end;

procedure WritelnWarning(const MessageBase: string;
  const Args: array of const);
begin
  WritelnWarning('', MessageBase, Args);
end;

initialization
finalization
  if LogStreamOwned then
  begin
    FreeAndNil(LogStream);
    FLog := false;
  end;
end.
