unit AI.Executor;

interface

uses
  SysUtils, Classes, AI.SharedTypes, AI.Functions;

type
  // Callback type for when an operation is completed
  // The string parameter will be the JSON serialized TFunctionCallResult
  TAsyncOperationCompletedEvent = procedure(Sender: TObject; const AResultJson: string) of object;

  TAIOperationExecutor = class
  private
    FOnOperationCompleted: TAsyncOperationCompletedEvent;
    procedure LogRequest(const Message: string); // Helper for logging

    // Helper methods for parameter extraction
    function GetParamValue(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: string = ''): string;
    function TryGetParamValue(Request: TFunctionCallRequest; const ParamName: string; out Value: string): Boolean;
    function GetParamValueAsInteger(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: Integer = 0; out ConversionOk: Boolean): Integer;
    function GetParamValueAsBoolean(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: Boolean = False; out ConversionOk: Boolean): Boolean;
    function RequestUserConfirmation(const OperationName: string; const Details: string): Boolean;

  public
    constructor Create;
    // This method will eventually become more complex, possibly handling async operations
    // For now, it's synchronous for simplicity in the stub.
    function ExecuteFunction(Request: TFunctionCallRequest): TFunctionCallResult;

    // For asynchronous execution (to be implemented properly later)
    // procedure ExecuteFunctionAsync(Request: TFunctionCallRequest);

    property OnOperationCompleted: TAsyncOperationCompletedEvent read FOnOperationCompleted write FOnOperationCompleted;
  end;

implementation

uses
  // For Windows-specific process listing (example) - ensure this is cross-platform or abstracted if needed
  {$IFDEF MSWINDOWS} Windows, TlHelp32, {$ENDIF}
  StrUtils, Math, // For ContainsText, SameText
  fpjson, jsonparser, // For JSON handling in listProcesses
  Dialogs, // For MessageDlg
  Generics.Collections, // For TObjectDictionary
  SyncObjs, // For TCriticalSection
  ProcessHandlerUnit, // To access global processhandler state
  NewKernelHandler,   // For actual process and memory functions
  CEFuncProc,         // For utility functions like getProcessnameFromProcessID and TScanSettings
  memscan,            // For TMemScan
  foundlisthelper,    // For TFoundList to retrieve scan results
  MainUnit;           // For global MainForm instance to access AddressList

type
  // Forward declare if used by TCheatTableEntry before its definition
  // TCheatTableEntry = class; (Not needed if defined before use)

  // Structure for simulated cheat table entries
  TSimulatedCheatEntry = class
  public
    Address: string;
    DataType: string; // 'Type' is a keyword, so using DataType
    Description: string;
    Value: string;     // Simulated current value at address
    IsFrozen: Boolean;
    constructor Create(AAddress, ADataType, ADescription: string);
  end;

  TScanTaskStatus = (stsPending, stsRunning, stsCompleted, stsError, stsCancelled);

  TScanTask = class
  public
    TaskId: string;
    Status: TScanTaskStatus;        // This will be updated by TMemScan events or polling
    ErrorMessage: string;
    ScanValue: string;              // Parameters used to start the scan
    ScanValue2: string;
    ScanTypeString: string;         // e.g., "4 Bytes", "Float"
    ScanOptionString: string;       // e.g., "Exact Value"
    CreationTime: TDateTime;
    LastAccessTime: TDateTime;
    MemScanInstance: TMemScan;      // The actual CE MemScan object for this task
    ProgressBar: TCustomProgressBar; // Dummy progress bar for TMemScan constructor

    constructor Create(AId: string; AValue, AValue2, ATypeStr, AOptionStr: string; AMemScan: TMemScan);
    destructor Destroy; override;

    // SimulateProgress will be removed, actual progress comes from MemScanInstance
    // FoundCount and Results will also be retrieved from MemScanInstance when completed
  end;

{ TAIOperationExecutor }
  TAIOperationExecutor = class
  private
    FOnOperationCompleted: TAsyncOperationCompletedEvent;
    FActiveScanTasks: TObjectDictionary<string, TScanTask>;
    FScanTaskLock: TCriticalSection; // To protect FActiveScanTasks
    FNextTaskId: Integer; // For scan tasks

    // Simulated state for attached process
    FCurrentAttachedPID: Integer;
    FCurrentAttachedProcessName: string;

    // Simulated cheat table (REMOVED)
    // FSimulatedCheatTable: TObjectList;
    // FCheatTableLock: TCriticalSection;

    procedure LogRequest(const Message: string); // Helper for logging

    // Helper methods for parameter extraction
    function GetParamValue(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: string = ''): string;
    function TryGetParamValue(Request: TFunctionCallRequest; const ParamName: string; out Value: string): Boolean;
    function GetParamValueAsInteger(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: Integer = 0; out ConversionOk: Boolean): Integer;
    function GetParamValueAsBoolean(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: Boolean = False; out ConversionOk: Boolean): Boolean;
    function RequestUserConfirmation(const OperationName: string; const Details: string): Boolean;

    procedure ExecuteStartFirstScan(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    procedure ExecuteGetScanStatus(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    procedure ExecuteGetScanResults(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    procedure ExecuteClearScan(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    function GenerateTaskId: string;

    procedure ExecuteAddAddressToCheatTable(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    procedure ExecuteGetCheatTableEntries(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    procedure ExecuteSetCheatTableEntryValue(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    procedure ExecuteSetCheatTableEntryDescription(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    procedure ExecuteToggleFreezeAddress(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
    // Add declarations for other cheat table functions here later (get, set value, etc.)

  public
    constructor Create;
    destructor Destroy; override;
begin // TAIOperationExecutor.Create
  inherited Create;
  FActiveScanTasks := TObjectDictionary<string, TScanTask>.Create([doOwnsValues]);
  FScanTaskLock := TCriticalSection.Create;
  FNextTaskId := 0;

  FCurrentAttachedPID := 0; // No process attached initially
  FCurrentAttachedProcessName := '';

  // FSimulatedCheatTable := TObjectList.Create(True); // REMOVED
  // FCheatTableLock := TCriticalSection.Create; // REMOVED

  LogRequest('TAIOperationExecutor Created.');
end;

destructor TAIOperationExecutor.Destroy;
begin
  LogRequest('TAIOperationExecutor Destroying...');
  FScanTaskLock.Enter;
  try
    FActiveScanTasks.Clear;
  finally
    FScanTaskLock.Leave;
  end;
  FreeAndNil(FActiveScanTasks);
  FreeAndNil(FScanTaskLock);

  // FCheatTableLock.Enter; // REMOVED
  // try // REMOVED
  //   FSimulatedCheatTable.Clear; // REMOVED
  // finally // REMOVED
  //   FCheatTableLock.Leave; // REMOVED
  // end; // REMOVED
  // FreeAndNil(FSimulatedCheatTable); // REMOVED
  // FreeAndNil(FCheatTableLock); // REMOVED

  LogRequest('TAIOperationExecutor Destroyed.');
  inherited Destroy;
end;

{ TSimulatedCheatEntry }
constructor TSimulatedCheatEntry.Create(AAddress, ADataType, ADescription: string);
begin
  inherited Create;
  Self.Address := AAddress;
  Self.DataType := ADataType;
  Self.Description := ADescription;
  Self.Value := '?'; // Default unknown value
  Self.IsFrozen := False;
end;

function TAIOperationExecutor.GenerateTaskId: string;
begin
  FScanTaskLock.Enter;
  try
    Inc(FNextTaskId);
    Result := 'scan_' + IntToStr(FNextTaskId);
  finally
    FScanTaskLock.Leave;
  end;
end;

{ TScanTask }
constructor TScanTask.Create(AId: string; AValue, AValue2, ATypeStr, AOptionStr: string; AMemScan: TMemScan);
begin
  inherited Create;
  TaskId := AId;
  ScanValue := AValue;
  ScanValue2 := AValue2;
  ScanTypeString := ATypeStr;
  ScanOptionString := AOptionStr;
  MemScanInstance := AMemScan; // Store the provided TMemScan instance

  Status := stsPending; // Initial status, will be updated by MemScanInstance events/polling
  ErrorMessage := '';
  CreationTime := Now;
  LastAccessTime := CreationTime;

  // ProgressBar is created and owned by this TScanTask, passed to TMemScan
  // This is a dummy progress bar not linked to UI, for TMemScan's constructor.
  // In a real UI app, this might be a real progress bar component.
  ProgressBar := TCustomProgressBar.Create(nil); // Create a dummy one

  // Assign self to MemScanInstance.Tag to find this TScanTask from MemScanInstance's events
  if Assigned(MemScanInstance) then
  begin
    MemScanInstance.Tag := PtrInt(Self); // Store reference to self
    // Set OnScanDone and OnGuiUpdate handlers on MemScanInstance here or in ExecuteStartFirstScan
    // For now, assuming it will be set up in ExecuteStartFirstScan before scan starts.
  end;

  LogRequest(Format('TScanTask %s created, associated with TMemScan instance. Value: "%s", Type: "%s"', [TaskId, ScanValue, ScanTypeString]));
end;

destructor TScanTask.Destroy;
begin
  LogRequest(Format('TScanTask %s being destroyed.', [TaskId]));
  if Assigned(MemScanInstance) then
  begin
    // Terminate scan if running, wait, then free.
    // Or, if TMemScan handles its own threads gracefully on Free, just Free is enough.
    // TMemScan's destructor should handle its TScanController thread.
    LogRequest(Format('Freeing associated TMemScan instance for task %s.', [TaskId]));
    FreeAndNil(MemScanInstance);
  end;
  FreeAndNil(ProgressBar); // Free the dummy progress bar
  inherited Destroy;
end;

// SimulateProgress is removed. Actual progress will be fetched from MemScanInstance.

procedure TAIOperationExecutor.LogRequest(const Message: string);
begin
  // In a real application, this would go to a log file or a debug console
  {$IFDEF DEBUG}
  OutputDebugString(PChar('[AI.Executor] ' + Message));
  {$ENDIF}
  // For now, also write to console if available, or could use a TMemo in UI for logs
  // writeln('[AI.Executor] ' + Message);
end;

function TAIOperationExecutor.TryGetParamValue(Request: TFunctionCallRequest; const ParamName: string; out Value: string): Boolean;
var
  I: Integer;
  Param: TFunctionCallParameter;
begin
  Result := False;
  Value := '';
  for I := 0 to Request.Parameters.Count - 1 do
  begin
    Param := Request.Parameters[I] as TFunctionCallParameter;
    if SameText(Param.Name, ParamName) then
    begin
      Value := Param.Value;
      Result := True;
      Exit;
    end;
  end;
end;

function TAIOperationExecutor.GetParamValue(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: string): string;
begin
  if not TryGetParamValue(Request, ParamName, Result) then
    Result := DefaultValue;
end;

function TAIOperationExecutor.GetParamValueAsInteger(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: Integer; out ConversionOk: Boolean): Integer;
var
  StrVal: string;
begin
  ConversionOk := False;
  Result := DefaultValue;
  if TryGetParamValue(Request, ParamName, StrVal) then
  begin
    ConversionOk := TryStrToInt(StrVal, Result);
    if not ConversionOk then Result := DefaultValue; // Reset to default if conversion failed
  end;
end;

function TAIOperationExecutor.GetParamValueAsBoolean(Request: TFunctionCallRequest; const ParamName: string; DefaultValue: Boolean; out ConversionOk: Boolean): Boolean;
var
  StrVal: string;
begin
  ConversionOk := False;
  Result := DefaultValue;
  if TryGetParamValue(Request, ParamName, StrVal) then
  begin
    if SameText(StrVal, 'true') or (StrVal = '1') then
    begin
      Result := True;
      ConversionOk := True;
    end
    else if SameText(StrVal, 'false') or (StrVal = '0') then
    begin
      Result := False;
      ConversionOk := True;
    end
    else // Conversion failed
    begin
      Result := DefaultValue; // Reset to default
      ConversionOk := False;
    end;
  end;
end;

// Synchronous execution for now. Will be refactored for async.
function TAIOperationExecutor.ExecuteFunction(Request: TFunctionCallRequest): TFunctionCallResult;
var
  LFunctionName: string;
  LResultData: string; // This string will hold the JSON formatted data specific to the function's output
  LStatus: string;
  LErrorMessage: string;
  // Specific variables for functions
  {$IFDEF MSWINDOWS}
  SnapshotHandle: THandle;
  ProcessEntry: TProcessEntry32;
  ProcessesArrayJson: TJSONArray; // Use TJSONArray from fpjson
  ProcessJsonObj: TJSONObject;
  {$ENDIF}
  PID: Integer;
  ConversionOk: Boolean;
  TempJsonObj: TJSONObject; // For constructing LResultData as JSON string

begin
  LFunctionName := Request.FunctionName;
  LResultData := '{}'; // Default empty JSON object string for ResultData
  LStatus := 'success';
  LErrorMessage := '';

  LogRequest(Format('Attempting to execute function: %s (ReqID: %s, ToolCallID: %s)', [LFunctionName, Request.RequestId, Request.ToolCallId]));

  // Dispatch to specific handlers
  if CompareText(LFunctionName, 'listProcesses') = 0 then
  begin
    LogRequest('Executing listProcesses...');
    ProcessesArrayJson := TJSONArray.Create; // Ensure this is TJSONArray from fpjson
    LStatus := 'success'; // Assume success unless an error occurs
    try
      {$IFDEF MSWINDOWS}
      SnapshotHandle := NewKernelHandler.CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if SnapshotHandle = INVALID_HANDLE_VALUE then
      begin
        LStatus := 'error';
        LErrorMessage := 'Failed to create snapshot of processes. WinAPI Error: ' + SysErrorMessage(GetLastError);
      end
      else
      try
        ProcessEntry.dwSize := SizeOf(TProcessEntry32); // TProcessEntry32 from NewKernelHandler
        if NewKernelHandler.Process32First(SnapshotHandle, ProcessEntry) then
        begin
          repeat
            ProcessJsonObj := TJSONObject.Create;
            ProcessJsonObj.Add('pid', ProcessEntry.th32ProcessID);
            ProcessJsonObj.Add('name', string(ProcessEntry.szExeFile));
            ProcessesArrayJson.Add(ProcessJsonObj);
          until not NewKernelHandler.Process32Next(SnapshotHandle, ProcessEntry);
        end
        else
        begin
          LStatus := 'error';
          LErrorMessage := 'Failed to get first process. WinAPI Error: ' + SysErrorMessage(GetLastError);
        end;
      finally
        NewKernelHandler.CloseHandle(SnapshotHandle); // Use NewKernelHandler's CloseHandle
      end;
      {$ELSE}
      // Basic non-windows placeholder - CE's GetProcessList in CEFuncProc might have more advanced mac/linux logic
      // For now, this AI part will return empty on non-windows if not using full CEFuncProc logic.
      // A more complete implementation would replicate or call CEFuncProc.GetProcessList logic here.
      LStatus := 'error';
      LErrorMessage := 'listProcesses is not fully implemented for non-Windows in AI.Executor yet.';
      {$ENDIF}

      if LStatus = 'success' then
      begin
        TempJsonObj := TJSONObject.Create;
        TempJsonObj.Add('processes', ProcessesArrayJson); // ProcessesArrayJson is owned by TempJsonObj
        LResultData := TempJsonObj.AsJSON;
        TempJsonObj.Free;
      end
      else // Error occurred or not windows
      begin
        ProcessesArrayJson.Free; // Clean up if not passed to TempJsonObj
        if LErrorMessage = '' then LErrorMessage := 'Failed to list processes.'; // Generic if not set
        LResultData := '{"processes": []}';
      end;
    except
      on E: Exception do
      begin
        LStatus := 'error';
        LErrorMessage := 'Exception during listProcesses: ' + E.Message;
        FreeAndNil(ProcessesArrayJson);
        LResultData := '{"processes": []}';
      end;
    end;
  end
  else if CompareText(LFunctionName, 'openProcess') = 0 then
  begin
    LogRequest('Executing openProcess (sensitive operation)...');
    PID := GetParamValueAsInteger(Request, 'pid', 0, ConversionOk); // Default to 0 if not found or invalid

    if not ConversionOk or (PID = 0) then // PID 0 is current process, typically not what user wants to "open"
    begin
      LStatus := 'error';
      LErrorMessage := 'Invalid or missing "pid" parameter for openProcess. Must be a valid non-zero integer.';
    end
    else
    begin
      if not RequestUserConfirmation('Open Process', Format('The AI assistant wants to open process with PID: %d. Do you allow this?', [PID])) then
      begin
        LStatus := 'error';
        LErrorMessage := 'Operation cancelled by user.';
        LResultData := '{"message": "User cancelled opening process."}';
      end
      else
      begin
        LogRequest(Format('User confirmed. Attempting to open process with PID: %d', [PID]));
        Dim TargetProcessHandle: THandle;
        Dim ProcessName: string;
        Dim DesiredAccess: DWORD;

        {$IFDEF MSWINDOWS}
        // PROCESS_ALL_ACCESS might be too much, CE uses a specific set.
        // From CEFuncProc.Open_Process: ifthen(GetSystemType<=6,$1f0fff, process_all_access)
        // $1f0fff = SYNCHRONIZE | STANDARD_RIGHTS_REQUIRED | PROCESS_VM_READ | PROCESS_VM_WRITE | PROCESS_VM_OPERATION | PROCESS_QUERY_INFORMATION | PROCESS_CREATE_THREAD | PROCESS_DUP_HANDLE | PROCESS_TERMINATE
        // Let's use PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | PROCESS_VM_WRITE | PROCESS_VM_OPERATION for now.
        DesiredAccess := PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_VM_WRITE or PROCESS_VM_OPERATION;
        if GetSystemType >= 6 then // Vista+
            DesiredAccess := PROCESS_ALL_ACCESS; // Matches CE's newer logic
        else
            DesiredAccess := $1F0FFF; // Older systems

        {$ELSE}
        DesiredAccess := $FFFFFFFF; // Placeholder for other OS, should use appropriate flags
        {$ENDIF}

        TargetProcessHandle := NewKernelHandler.OpenProcess(DesiredAccess, FALSE, PID);

        if (TargetProcessHandle <> 0) and (TargetProcessHandle <> INVALID_HANDLE_VALUE) then
        begin
          // Successfully opened, now update global ProcessHandler
          ProcessHandlerUnit.processhandler.processid := PID; // Set PID first
          ProcessHandlerUnit.processhandler.ProcessHandle := TargetProcessHandle; // This setter updates is64bit etc.

          ProcessName := CEFuncProc.getProcessnameFromProcessID(PID); // Use CEFuncProc utility
          if ProcessName = '???' then ProcessName := Format('Process %d', [PID]);


          LStatus := 'success';
          LResultData := Format('{"message": "Successfully attached to process %s (PID: %d)"}', [ProcessName, PID]);
          LogRequest(Format('Successfully attached to process %s (PID: %d). Global ProcessHandler updated.', [ProcessName, PID]));
        end
        else
        begin
          LStatus := 'error';
          LErrorMessage := 'Failed to open process PID ' + IntToStr(PID) + '. WinAPI Error: ' + SysErrorMessage(GetLastError);
          LResultData := Format('{"message": "Failed to attach to process PID %d. %s"}', [PID, LErrorMessage]);
          // Ensure global processhandler is not pointing to a failed handle
          if ProcessHandlerUnit.processhandler.processid = PID then
          begin
             ProcessHandlerUnit.processhandler.ProcessHandle := 0; // Effectively closes it in setter
             ProcessHandlerUnit.processhandler.processid := 0;
          end;
        end;
      end;
    end;
  end
  else if CompareText(LFunctionName, 'getAttachedProcess') = 0 then
  begin
    LogRequest('Executing getAttachedProcess...');
    Dim AttachedPID: DWORD;
    Dim AttachedProcessName: string;
    Dim JsonProcInfo: TJSONObject;

    AttachedPID := ProcessHandlerUnit.processhandler.processid;

    if AttachedPID <> 0 then
    begin
      // It's possible processhandler.ProcessName is already set by its setter,
      // but getProcessnameFromProcessID is more direct if we only have PID.
      AttachedProcessName := CEFuncProc.getProcessnameFromProcessID(AttachedPID);
      if AttachedProcessName = '???' then AttachedProcessName := Format('Process %d', [AttachedPID]);

      JsonProcInfo := TJSONObject.Create;
      try
        JsonProcInfo.Add('pid', AttachedPID);
        JsonProcInfo.Add('name', AttachedProcessName);
        LResultData := Format('{"process": %s}', [JsonProcInfo.AsJSON]);
      finally
        JsonProcInfo.Free;
      end;
      LStatus := 'success';
    end
    else
    begin
      LResultData := '{"process": null}';
      LStatus := 'success'; // Successful query, no process attached
    end;
  end
  // Scan Function Handling
  else if CompareText(LFunctionName, 'startFirstScan') = 0 then
  begin
    ExecuteStartFirstScan(Request, LResultData, LStatus, LErrorMessage);
  end
  else if CompareText(LFunctionName, 'getScanStatus') = 0 then
  begin
    ExecuteGetScanStatus(Request, LResultData, LStatus, LErrorMessage);
  end
  else if CompareText(LFunctionName, 'getScanResults') = 0 then
  begin
    ExecuteGetScanResults(Request, LResultData, LStatus, LErrorMessage);
  end
  else if CompareText(LFunctionName, 'clearScan') = 0 then
  begin
    ExecuteClearScan(Request, LResultData, LStatus, LErrorMessage);
  end
  // Cheat Table functions
  else if CompareText(LFunctionName, 'addAddressToCheatTable') = 0 then
  begin
    ExecuteAddAddressToCheatTable(Request, LResultData, LStatus, LErrorMessage);
  end
  else if CompareText(LFunctionName, 'getCheatTableEntries') = 0 then
  begin
    ExecuteGetCheatTableEntries(Request, LResultData, LStatus, LErrorMessage);
  end
  else if CompareText(LFunctionName, 'setCheatTableEntryValue') = 0 then
  begin
    ExecuteSetCheatTableEntryValue(Request, LResultData, LStatus, LErrorMessage);
  end
  else if CompareText(LFunctionName, 'setCheatTableEntryDescription') = 0 then
  begin
    ExecuteSetCheatTableEntryDescription(Request, LResultData, LStatus, LErrorMessage);
  end
  else
  begin
    LStatus := 'error';
    LErrorMessage := Format('Function "%s" is not implemented in AI.Executor stub.', [LFunctionName]);
    LogRequest(LErrorMessage);
  end;

  // Ensure RequestId and ToolCallId from the original request are passed to the result
  Result := TFunctionCallResult.Create(LFunctionName, LStatus, LResultData, Request.RequestId, Request.ToolCallId, LErrorMessage);
  LogRequest(Format('Execution of %s finished. Status: %s', [LFunctionName, LStatus]));
end;

procedure TAIOperationExecutor.ExecuteAddAddressToCheatTable(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  AddressStr, DataTypeStr, DescriptionStr, CustomTypeName: string;
  CEVarType: TVariableType;
  MemRec: TMemoryRecord;
  EntryIndex: Integer;
  JsonObj: TJSONObject;
  AddressLength, BitStart: Integer; // For string/binary types
  IsUnicode : Boolean;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';

  AddressStr := GetParamValue(Request, 'address');
  DataTypeStr := GetParamValue(Request, 'type');
  DescriptionStr := GetParamValue(Request, 'description', ''); // Optional

  if (AddressStr = '') or (DataTypeStr = '') then
  begin
    LStatus := 'error';
    LErrorMessage := 'Missing required parameters for addAddressToCheatTable (address, type).';
    LogRequest(LErrorMessage);
    Exit;
  end;

  // Validate address format (basic check for 0x prefix and hex)
  if not ( (Length(Address) > 2) and (Address[1] = '0') and ((Address[2] = 'x') or (Address[2] = 'X')) ) then
  begin
     // Could also try to convert to pointer/integer to validate further
     // For now, just a prefix check.
     // More robust: use TryStrToInt64 with '$' prefix after stripping '0x'.
  end;
  // Further validation for DataType could be added here (e.g., check against known types)


  // User Confirmation for adding to cheat table
  if not RequestUserConfirmation('Add Address to Cheat Table', Format('AI wants to add address: %s (Type: %s, Desc: "%s") to the cheat table. Allow?', [Address, DataType, Description])) then
  begin
    LStatus := 'error';
    LErrorMessage := 'Operation cancelled by user.';
    LResultData := '{"message": "User cancelled adding address to cheat table."}';
    Exit;
  end;

  NewEntry := TSimulatedCheatEntry.Create(Address, DataType, Description);

  FCheatTableLock.Enter;
  try
    EntryIndex := FSimulatedCheatTable.Add(NewEntry); // TObjectList takes ownership
  finally
    FCheatTableLock.Leave;
  end;

  JsonObj := TJSONObject.Create;
  try
    JsonObj.Add('index', EntryIndex);
    LResultData := JsonObj.AsJSON;
  finally
    JsonObj.Free;
  end;
  LogRequest(Format('Retrieved %d entries from cheat table.', [EntriesArrayJson.Count]));
end;

procedure TAIOperationExecutor.ExecuteSetCheatTableEntryValue(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  EntryIndex, OriginalIntValue: Integer; // For displaying current value if it's an int
  NewValueStr, CurrentValueStr, ConfirmDetails: string;
  MemRec: TMemoryRecord;
  ConversionOk: Boolean;
  JsonObj: TJSONObject;
  OriginalFloatValue: Double;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}'; // Default success, no data needed back other than status

  EntryIndex := GetParamValueAsInteger(Request, 'index', -1, ConversionOk);
  NewValueStr := GetParamValue(Request, 'value');

  if not ConversionOk or (EntryIndex < 0) then
  begin
    LStatus := 'error'; LErrorMessage := 'Invalid or missing "index" parameter.';
    LogRequest(LErrorMessage); Exit;
  end;

  if NewValueStr = '' then // Or other validation for value string
  begin
    LStatus := 'error'; LErrorMessage := 'Missing "value" parameter.';
    LogRequest(LErrorMessage); Exit;
  end;

  if not Assigned(MainForm) or not Assigned(MainForm.AddressList) or (EntryIndex >= MainForm.AddressList.Count) then
  begin
    LStatus := 'error'; LErrorMessage := Format('Invalid entry index %d or AddressList not available.', [EntryIndex]);
    LogRequest(LErrorMessage); Exit;
  end;

  MemRec := MainForm.AddressList.MemRecItems[EntryIndex];
  if not Assigned(MemRec) then
  begin
    LStatus := 'error'; LErrorMessage := Format('No memory record found at index %d.', [EntryIndex]);
    LogRequest(LErrorMessage); Exit;
  end;

  // Get current value for confirmation dialog
  try
    CurrentValueStr := MemRec.GetValue; // This reads memory and formats
  except
    on E: Exception do CurrentValueStr := '(Error reading current value)';
  end;

  ConfirmDetails := Format('Entry (Index %d): %s'#13#10'Current Value: %s'#13#10'New Value: %s',
                           [EntryIndex, MemRec.Description, CurrentValueStr, NewValueStr]);

  if not RequestUserConfirmation('Set Cheat Table Entry Value', ConfirmDetails) then
  begin
    LStatus := 'error'; LErrorMessage := 'Operation cancelled by user.';
    LResultData := '{"message": "User cancelled setting entry value."}'; Exit;
  end;

  try
    MemRec.SetValue(NewValueStr); // TMemoryRecord.SetValue handles parsing and writing
    // MemRec.Treenode.Repaint; // or MainForm.AddressList.Refresh; if needed
    LogRequest(Format('Value for entry %d ("%s") set to "%s".', [EntryIndex, MemRec.Description, NewValueStr]));
    LResultData := '{"message": "Value set successfully."}'; // Optional success detail
  except
    on E: Exception do
    begin
      LStatus := 'error'; LErrorMessage := 'Failed to set value for entry ' + IntToStr(EntryIndex) + ': ' + E.Message;
      LogRequest(LErrorMessage);
    end;
  end;
end;

procedure TAIOperationExecutor.ExecuteGetCheatTableEntries(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  EntriesArrayJson: TJSONArray;
  EntryJsonObj: TJSONObject;
  SimEntry: TSimulatedCheatEntry;
  I: Integer;
  RootJsonObj: TJSONObject;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LogRequest('Executing getCheatTableEntries...');

  EntriesArrayJson := TJSONArray.Create;
  FCheatTableLock.Enter;
  try
    for I := 0 to FSimulatedCheatTable.Count - 1 do
    begin
      SimEntry := FSimulatedCheatTable[I] as TSimulatedCheatEntry;
      EntryJsonObj := TJSONObject.Create;
      EntryJsonObj.Add('index', I);
      EntryJsonObj.Add('address', SimEntry.Address);
      EntryJsonObj.Add('type', SimEntry.DataType);
      EntryJsonObj.Add('description', SimEntry.Description);
      EntryJsonObj.Add('value', SimEntry.Value); // Value is currently just '?'
      EntryJsonObj.Add('is_frozen', SimEntry.IsFrozen);
      EntriesArrayJson.Add(EntryJsonObj); // Array takes ownership
    end;
  finally
    FCheatTableLock.Leave;
  end;

  RootJsonObj := TJSONObject.Create;
  try
    RootJsonObj.Add('entries', EntriesArrayJson); // Root takes ownership of EntriesArrayJson
    LResultData := RootJsonObj.AsJSON;
  finally
    RootJsonObj.Free; // This frees RootJsonObj and EntriesArrayJson (and its children)
  end;
  LogRequest(Format('Retrieved %d entries from simulated cheat table.', [EntriesArrayJson.Count]));
end;

// Forward declaration for event handlers if they are methods of TAIOperationExecutor
// procedure TAIOperationExecutor_MemScanDone(Sender: TObject); // Example

procedure TAIOperationExecutor.ExecuteStartFirstScan(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  ScanValue, ScanValue2, ScanTypeStr, ScanOptionStr, TaskId: string;
  NewScanTask: TScanTask;
  NewMemScan: TMemScan;
  JsonObj: TJSONObject;
  CEVarType: TVariableType;
  CEScanOption: TScanOption;
  CERoundingType: TRoundingType;
  ScanStartAddr, ScanStopAddr: PtrUInt;
  IsHex, IsUnicode, IsCasesensitive, IsBinaryDecimal: Boolean;
  FastScanMethod: TFastScanMethod;
  FastScanParam: string;
  CustomT: TCustomType;

  procedure LocalMemScanDone(Sender: TObject);
  var
    Task: TScanTask;
    MemScan: TMemScan;
  begin
    MemScan := TMemScan(Sender);
    if MemScan.Tag <> 0 then // Tag should hold PtrInt(TScanTask)
    begin
      Task := TScanTask(PtrInt(MemScan.Tag));
      Self.FScanTaskLock.Enter; // Use Self to access TAIOperationExecutor's lock
      try
        if MemScan.hasError then
        begin
          Task.Status := stsError;
          Task.ErrorMessage := MemScan.GetErrorString;
          LogRequest(Format('Scan task %s ERRORED: %s', [Task.TaskId, Task.ErrorMessage]));
        end
        else
        begin
          Task.Status := stsCompleted;
          LogRequest(Format('Scan task %s COMPLETED.', [Task.TaskId]));
        end;
        Task.LastAccessTime := Now;
      finally
        Self.FScanTaskLock.Leave;
      end;
    end
    else
      LogRequest('LocalMemScanDone called for a TMemScan instance with no TScanTask Tag.');
  end;

  procedure LocalMemScanGuiUpdate(Sender: TObject; totaladdressestoscan: qword; currentlyscanned: qword; foundcount: qword);
  var
    Task: TScanTask;
    MemScan: TMemScan;
  begin
    MemScan := TMemScan(Sender);
    if MemScan.Tag <> 0 then
    begin
      Task := TScanTask(PtrInt(MemScan.Tag));
      // This is where progress would be updated if TScanTask stored it directly.
      // For now, getScanStatus will fetch progress from MemScanInstance.
      // LogRequest(Format('Scan task %s GUI Update: %d/%d, Found: %d', [Task.TaskId, currentlyscanned, totaladdressestoscan, foundcount]));
    end;
  end;

begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';

  ScanValue := GetParamValue(Request, 'value');
  ScanTypeStr := GetParamValue(Request, 'type');
  ScanOptionStr := GetParamValue(Request, 'scan_option');
  ScanValue2 := GetParamValue(Request, 'value2', ''); // Optional

  if (ScanValue = '') or (ScanTypeStr = '') or (ScanOptionStr = '') then
  begin
    LStatus := 'error';
    LErrorMessage := 'Missing required parameters for startFirstScan (value, type, scan_option).';
    LogRequest(LErrorMessage);
    Exit;
  end;

  // --- Convert AI string parameters to CE types ---
  CEVarType := CEFuncProc.StringToVariableType(ScanTypeStr); // Assuming CEFuncProc is in uses
  CustomT := nil;
  if CEVarType = vtCustom then
  begin
    // Need a way to get TCustomType instance if type string refers to one by name
    // For now, error if vtCustom is requested but not further specified or found
    LStatus := 'error'; LErrorMessage := 'Custom scan types require specific handling not yet implemented in AI executor.'; Exit;
  end;

  // Simplified mapping for CEScanOption and CERoundingType
  // A more robust mapping from ScanOptionStr to CEScanOption and CERoundingType is needed.
  if CompareText(ScanOptionStr, 'Exact Value') = 0 then CEScanOption := soExactValue
  else if CompareText(ScanOptionStr, 'Increased Value') = 0 then CEScanOption := soIncreasedValue
  else if CompareText(ScanOptionStr, 'Decreased Value') = 0 then CEScanOption := soDecreasedValue
  // ... add all other mappings from CEFuncProc scan constants ...
  else begin LStatus := 'error'; LErrorMessage := 'Unsupported scan_option: ' + ScanOptionStr; Exit; end;

  CERoundingType := rtRounded; // Default, may need to be configurable or inferred

  // Scan range (for now, use full range of attached process if available)
  if ProcessHandlerUnit.processhandler.processid <> 0 then
  begin
    // These would ideally come from CE's memory region enumeration for the process
    // For now, using placeholder full range. CE's TMemScan might do this internally.
    ScanStartAddr := GlobalScanSettings.StartAddress; // Use global CE settings
    ScanStopAddr := GlobalScanSettings.StopAddress;
  end else
  begin
    LStatus := 'error'; LErrorMessage := 'No process is currently open to perform a scan.'; Exit;
  end;

  // Other parameters (defaults for now)
  IsHex := Pos('$', ScanValue) = 1; // Basic check, CE's parsing is more robust
  IsUnicode := SameText(ScanTypeStr, 'Unicode String'); // Or from a specific param
  IsCasesensitive := False; // Default, could be a parameter
  IsBinaryDecimal := False; // For binary scans, default to binary input
  FastScanMethod := fsmNotAligned; // Default
  FastScanParam := '';

  // User Confirmation
  if not RequestUserConfirmation('Start Memory Scan', Format('AI wants to start a new memory scan for "%s" (Type: %s, Option: %s). Allow?', [ScanValue, ScanTypeStr, ScanOptionStr])) then
  begin
    LStatus := 'error'; LErrorMessage := 'Scan operation cancelled by user.';
    LResultData := '{"message": "User cancelled the scan operation."}'; Exit;
  end;

  TaskId := GenerateTaskId;
  NewMemScan := TMemScan.Create(nil); // Pass nil for progressbar, AI executor doesn't show UI
  try
    NewMemScan.GUIScanner := False; // Important: AI executor is not a GUI scanner in the same way TMainForm is
    NewMemScan.OnScanDone := @LocalMemScanDone; // Set our local handler
    NewMemScan.OnGuiUpdate := @LocalMemScanGuiUpdate; // Could be nil if not needed
    // Other TMemScan properties might need to be set based on AIConfig or defaults from CE
    NewMemScan.ScanWritable := GlobalScanSettings.scanWritable;
    NewMemScan.ScanExecutable := GlobalScanSettings.scanExecutable;
    NewMemScan.ScanCopyOnWrite := GlobalScanSettings.scanCopyOnWrite;
    // etc. for other global scan settings (MEM_PRIVATE, IMAGE, MAPPED)

    // Create the TScanTask to wrap this TMemScan instance
    NewScanTask := TScanTask.Create(TaskId, ScanValue, ScanValue2, ScanTypeStr, ScanOptionStr, NewMemScan);
    NewMemScan.Tag := PtrInt(NewScanTask); // So event handlers can find their TScanTask

    FScanTaskLock.Enter;
    try
      FActiveScanTasks.Add(TaskId, NewScanTask); // Dictionary takes ownership of NewScanTask
    finally
      FScanTaskLock.Leave;
    end;

    // Configure and start the actual scan
    // This is a simplified call; TMainForm does more setup on NewMemScan properties.
    // We need to replicate that or ensure TMemScan defaults are okay.
    NewMemScan.FirstScan(
      CEScanOption, CEVarType, CERoundingType, ScanValue, ScanValue2,
      ScanStartAddr, ScanStopAddr, IsHex, IsBinaryDecimal, IsUnicode, IsCasesensitive,
      FastScanMethod, FastScanParam, CustomT
    );
    // FirstScan is asynchronous and will use its own threads.

    NewScanTask.Status := stsRunning; // Mark as running (will be updated by OnScanDone)
    NewScanTask.LastAccessTime := Now;

    JsonObj := TJSONObject.Create;
    try
      JsonObj.Add('task_id', TaskId);
      LResultData := JsonObj.AsJSON;
    finally
      JsonObj.Free;
    end;
    LogRequest(Format('Real scan task %s initiated for value "%s", type "%s".', [TaskId, ScanValue, ScanTypeStr]));

  except
    on E: Exception do
    begin
      LStatus := 'error';
      LErrorMessage := 'Failed to start scan: ' + E.Message;
      LogRequest(LErrorMessage);
      if Assigned(NewMemScan) then NewMemScan.Free; // Free if created but scan start failed
      // NewScanTask might not have been created or added.
    end;
  end;
end;

procedure TAIOperationExecutor.ExecuteGetScanStatus(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  TaskId: string;
  Task: TScanTask;
  JsonObj: TJSONObject;
  TotalToScan, CurrentScanned, Found: QWord;
  ProgressPercentage: Double;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';
  TaskId := GetParamValue(Request, 'task_id');

  if TaskId = '' then
  begin
    LStatus := 'error'; LErrorMessage := 'Missing task_id parameter for getScanStatus.';
    LogRequest(LErrorMessage); Exit;
  end;

  FScanTaskLock.Enter;
  try
    if not FActiveScanTasks.TryGetValue(TaskId, Task) then
    begin
      LStatus := 'error'; LErrorMessage := Format('Scan task with ID "%s" not found.', [TaskId]);
      LogRequest(LErrorMessage); Exit;
    end;

    // Task.Status is updated by OnScanDone handler.
    // For progress, query the MemScanInstance directly.
    JsonObj := TJSONObject.Create;
    try
      Task.LastAccessTime := Now;
      if Assigned(Task.MemScanInstance) then
      begin
        if Task.Status = stsRunning then // Only get progress if TScanTask thinks it's running
        begin
          // Check if the underlying TMemScan's controller is actually done
          if Assigned(Task.MemScanInstance.scanController) and Task.MemScanInstance.scanController.isdone then
          begin
             // The OnScanDone event might not have fired yet or TScanTask status not updated.
             // Force update TScanTask status based on controller.
             if Task.MemScanInstance.scanController.hasError then
             begin
                Task.Status := stsError;
                Task.ErrorMessage := Task.MemScanInstance.scanController.errorstring;
             end else
                Task.Status := stsCompleted; // Mark as completed
             LogRequest(Format('Task %s controller was done but TScanTask status was running. Updated.', [Task.TaskId]));
          end
        end;

        case Task.Status of
          stsPending:   JsonObj.Add('status', 'pending');
          stsRunning:
            begin
              JsonObj.Add('status', 'running');
              Task.MemScanInstance.GetProgress(TotalToScan, CurrentScanned, Found);
              if TotalToScan > 0 then
                ProgressPercentage := CurrentScanned / TotalToScan
              else
                ProgressPercentage := 0;
              JsonObj.Add('progress', ProgressPercentage);
            end;
          stsCompleted:
            begin
              JsonObj.Add('status', 'completed');
              JsonObj.Add('found_count', Task.MemScanInstance.GetFoundCount);
            end;
          stsError:
            begin
              JsonObj.Add('status', 'error');
              JsonObj.Add('error_message', Task.ErrorMessage);
            end;
          stsCancelled: JsonObj.Add('status', 'cancelled'); // Note: Cancellation logic not yet implemented
        end;
      end else // No MemScanInstance, likely an issue during task creation
      begin
        JsonObj.Add('status', 'error');
        JsonObj.Add('error_message', 'Associated TMemScan instance is missing for task ' + TaskId);
      end;
      LResultData := JsonObj.AsJSON;
    finally
      JsonObj.Free;
    end;
  finally
    FScanTaskLock.Leave;
  end;
end;

procedure TAIOperationExecutor.ExecuteGetScanResults(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  TaskId: string;
  Task: TScanTask;
  Offset, Count, I, ActualCount, MaxResults: Integer;
  ConversionOk: Boolean;
  JsonObj: TJSONObject;
  ResultsArrayJson: TJSONArray;
  TempFoundList: TFoundList;
  Address: PtrUInt;
  Extra: DWord;
  ValueStr: string; // Max length of string representation of a value
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';
  TaskId := GetParamValue(Request, 'task_id');

  Offset := GetParamValueAsInteger(Request, 'offset', 0, ConversionOk);
  if not ConversionOk then Offset := 0;
  Count := GetParamValueAsInteger(Request, 'count', 100, ConversionOk); // Default to 100 results
  if not ConversionOk then Count := 100;
  if Count <= 0 then Count := 100;
  MaxResults := 1000; // Safety limit for AI requests
  if Count > MaxResults then Count := MaxResults;


  if TaskId = '' then
  begin
    LStatus := 'error'; LErrorMessage := 'Missing task_id parameter for getScanResults.';
    LogRequest(LErrorMessage); Exit;
  end;

  FScanTaskLock.Enter;
  try
    if not FActiveScanTasks.TryGetValue(TaskId, Task) then
    begin
      LStatus := 'error'; LErrorMessage := Format('Scan task with ID "%s" not found.', [TaskId]);
      LogRequest(LErrorMessage); Exit;
    end;

    if Task.Status <> stsCompleted then
    begin
      LStatus := 'error';
      LErrorMessage := Format('Scan task "%s" is not completed. Current status: %s.', [TaskId, GetEnumName(TypeInfo(TScanTaskStatus), Ord(Task.Status))]);
      LogRequest(LErrorMessage); Exit;
    end;

    if not Assigned(Task.MemScanInstance) then
    begin
      LStatus := 'error'; LErrorMessage := 'No TMemScan instance associated with task ' + TaskId;
      LogRequest(LErrorMessage); Exit;
    end;

    // Create a temporary TFoundList to read results
    // TFoundList constructor needs a TListView, which we don't have here.
    // This indicates TFoundList is tightly coupled with UI.
    // We need to replicate the logic of TFoundList.Initialize and result iteration,
    // or TMemScan needs to provide a direct way to get results.
    // TMemScan stores results in 'ADDRESSES.TMP' and 'MEMORY.TMP'.
    // For now, returning a placeholder as direct result file reading is complex.
    // TODO: Implement actual result retrieval from TMemScan's files.

    TempFoundList := TFoundList.Create(nil, Task.MemScanInstance); // nil for ListView, might not work fully
    try
      TempFoundList.Initialize(Task.MemScanInstance.VarType, Task.MemScanInstance.GetBinarySize,
                               Task.MemScanInstance.isHexadecimal, False, True, Task.MemScanInstance.isUnicode,
                               Task.MemScanInstance.CustomType); // Simplified parameters

      JsonObj := TJSONObject.Create;
      try
        ResultsArrayJson := TJSONArray.Create;
        ActualCount := 0;
        SetLength(ValueStr, 256); // Max length for value string representation

        // Loop through results using TFoundList (if it works without a ListView)
        // This is a conceptual loop; TFoundList might not support this iteration easily without a ListView.
        // A more direct file reading approach might be needed by replicating TFoundList's core logic.
        for I := 0 to TempFoundList.Count - 1 do
        begin
          if I < Offset then Continue; // Skip until offset
          if ActualCount >= Count then Break; // Got enough results

          Address := TempFoundList.GetAddress(I, Extra, ValueStr); // ValueStr is output here
          ResultsArrayJson.Add(IntToHex(Address, ProcessHandlerUnit.processhandler.pointersize*2)); // Format address as hex string
          Inc(ActualCount);
        end;

        JsonObj.Add('results', ResultsArrayJson);
        JsonObj.Add('total_found', TempFoundList.Count); // Or Task.MemScanInstance.GetFoundCount
        JsonObj.Add('offset_returned', Offset);
        JsonObj.Add('count_returned', ActualCount);
        LResultData := JsonObj.AsJSON;
        Task.LastAccessTime := Now;
      finally
        JsonObj.Free;
      end;
    finally
      TempFoundList.Deinitialize; // Important to close files
      TempFoundList.Free;
    end;

    // Fallback / Placeholder if TFoundList approach above is problematic without UI:
    if LResultData = '{}' then // if the above failed or was bypassed
    begin
        LogRequest(Format('Scan task %s: Actual result retrieval using TFoundList is complex without UI. Returning placeholder.', [TaskId]));
        LResultData := Format('{"message": "Result retrieval from scan %s not fully implemented in AI executor yet. Found count: %d", "total_found": %d, "results":[]}',
                              [TaskId, Task.MemScanInstance.GetFoundCount, Task.MemScanInstance.GetFoundCount]);
    end;

  finally
    FScanTaskLock.Leave;
  end;
end;

procedure TAIOperationExecutor.ExecuteSetCheatTableEntryDescription(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  EntryIndex: Integer;
  NewDescription: string;
  MemRec: TMemoryRecord;
  ConversionOk: Boolean;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';

  EntryIndex := GetParamValueAsInteger(Request, 'index', -1, ConversionOk);
  NewDescription := GetParamValue(Request, 'description'); // No default, empty is valid though.

  if not ConversionOk or (EntryIndex < 0) then
  begin
    LStatus := 'error'; LErrorMessage := 'Invalid or missing "index" parameter.';
    LogRequest(LErrorMessage); Exit;
  end;

  // Description can be empty, so no check for empty NewDescription string specifically.

  if not Assigned(MainForm) or not Assigned(MainForm.AddressList) or (EntryIndex >= MainForm.AddressList.Count) then
  begin
    LStatus := 'error'; LErrorMessage := Format('Invalid entry index %d or AddressList not available.', [EntryIndex]);
    LogRequest(LErrorMessage); Exit;
  end;

  MemRec := MainForm.AddressList.MemRecItems[EntryIndex];
  if not Assigned(MemRec) then
  begin
    LStatus := 'error'; LErrorMessage := Format('No memory record found at index %d.', [EntryIndex]);
    LogRequest(LErrorMessage); Exit;
  end;

  // No user confirmation needed for description change as per guide
  try
    MemRec.Description := NewDescription;
    if Assigned(MemRec.treenode) then MemRec.treenode.Repaint; // Request UI update
    LogRequest(Format('Description for entry %d set to "%s".', [EntryIndex, NewDescription]));
    LResultData := '{"message": "Description set successfully."}';
  except
    on E: Exception do
    begin
      LStatus := 'error'; LErrorMessage := 'Failed to set description for entry ' + IntToStr(EntryIndex) + ': ' + E.Message;
      LogRequest(LErrorMessage);
    end;
  end;
end;

procedure TAIOperationExecutor.ExecuteToggleFreezeAddress(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  EntryIndex: Integer;
  FreezeState, ConversionOk: Boolean;
  MemRec: TMemoryRecord;
  ActionStr: string;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';

  EntryIndex := GetParamValueAsInteger(Request, 'index', -1, ConversionOk);
  if not ConversionOk or (EntryIndex < 0) then
  begin
    LStatus := 'error'; LErrorMessage := 'Invalid or missing "index" parameter.';
    LogRequest(LErrorMessage); Exit;
  end;

  FreezeState := GetParamValueAsBoolean(Request, 'freeze', False, ConversionOk);
  if not ConversionOk then
  begin
    LStatus := 'error'; LErrorMessage := 'Invalid "freeze" parameter (must be true or false).';
    LogRequest(LErrorMessage); Exit;
  end;

  if not Assigned(MainForm) or not Assigned(MainForm.AddressList) or (EntryIndex >= MainForm.AddressList.Count) then
  begin
    LStatus := 'error'; LErrorMessage := Format('Invalid entry index %d or AddressList not available.', [EntryIndex]);
    LogRequest(LErrorMessage); Exit;
  end;

  MemRec := MainForm.AddressList.MemRecItems[EntryIndex];
  if not Assigned(MemRec) then
  begin
    LStatus := 'error'; LErrorMessage := Format('No memory record found at index %d.', [EntryIndex]);
    LogRequest(LErrorMessage); Exit;
  end;

  if MemRec.VarType = vtAutoAssembler then // Freezing AA scripts is more like activating/deactivating
  begin
      if FreezeState then ActionStr := 'activate AA script' else ActionStr := 'deactivate AA script';
  end else
  begin
      if FreezeState then ActionStr := 'FREEZE' else ActionStr := 'UNFREEZE';
  end;


  if not RequestUserConfirmation('Toggle Freeze Address', Format('AI wants to %s entry %d ("%s"). Allow?', [ActionStr, EntryIndex, MemRec.Description])) then
  begin
    LStatus := 'error'; LErrorMessage := 'Operation cancelled by user.';
    LResultData := '{"message": "User cancelled toggling freeze state."}'; Exit;
  end;

  try
    // For normal records, MemRec.Active handles the freeze state.
    // The TMainForm.FreezeTimerTimer or TFreezeThread then calls MemRec.ApplyFreeze.
    // Setting MemRec.Active is sufficient to enable/disable this.
    // For AA scripts, Active also means running the enable/disable section.
    MemRec.Active := FreezeState;

    // If it's an AA script and we are activating, it might run asynchronously.
    // If it's a normal variable being frozen, it might need to read the current value.
    if FreezeState and (MemRec.VarType <> vtAutoAssembler) then
    begin
        MemRec.FrozenValue := MemRec.GetValue; // Store current value for freezing
    end;

    if Assigned(MemRec.treenode) then MemRec.treenode.Repaint;
    LogRequest(Format('%s for entry %d ("%s") set to %s.', [ActionStr, EntryIndex, MemRec.Description, BoolToStr(FreezeState, True)]));
    LResultData := Format('{"message": "Entry %s state set to %s."}', [ActionStr, BoolToStr(FreezeState, True)]);
  except
    on E: Exception do
    begin
      LStatus := 'error'; LErrorMessage := Format('Failed to %s for entry %d: %s', [ActionStr, EntryIndex, E.Message]);
      LogRequest(LErrorMessage);
    end;
  end;
end;

procedure TAIOperationExecutor.ExecuteClearScan(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  TaskId: string;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}'; // Simple success message usually fine, e.g. {"message": "Task cleared"}
  TaskId := GetParamValue(Request, 'task_id');

  if TaskId = '' then
  begin
    LStatus := 'error';
    LErrorMessage := 'Missing task_id parameter for clearScan.';
    LogRequest(LErrorMessage);
    Exit;
  end;

  FScanTaskLock.Enter;
  try
    if FActiveScanTasks.ContainsKey(TaskId) then
    begin
      FActiveScanTasks.Remove(TaskId); // This will free the TScanTask due to doOwnsValues
      LResultData := '{"message": "Scan task ' + TaskId + ' cleared successfully."}';
      LogRequest(Format('Scan task %s cleared.', [TaskId]));
    end
    else
    begin
      LStatus := 'error';
      LErrorMessage := Format('Scan task with ID "%s" not found for clearing.', [TaskId]);
      LogRequest(LErrorMessage);
    end;
  finally
    FScanTaskLock.Leave;
  end;
end;

function TAIOperationExecutor.RequestUserConfirmation(const OperationName: string; const Details: string): Boolean;
var
  DialogResult: Integer;
  Msg: string;
begin
  LogRequest(Format('Requesting user confirmation for: %s - Details: %s', [OperationName, Details]));
  Msg := Format('The AI Assistant wants to perform the following operation:' + sLineBreak + sLineBreak +
                'Operation: %s' + sLineBreak +
                'Details: %s' + sLineBreak + sLineBreak +
                'Do you want to allow this action?', [OperationName, Details]);

  // Ensure this runs in the main thread if TAIOperationExecutor methods can be called from non-main threads.
  // For now, assuming ExecuteFunction is called in a context where UI dialogs are safe.
  // If not, this would need to be TThread.Synchronize or similar.
  DialogResult := MessageDlg('AI Action Confirmation', Msg, mtConfirmation, [mbYes, mbNo], 0); // mbNo is default

  Result := (DialogResult = mrYes);
  if Result then
    LogRequest('User CONFIRMED action: ' + OperationName)
  else
    LogRequest('User DENIED action: ' + OperationName);
end;

// procedure TAIOperationExecutor.ExecuteFunctionAsync(Request: TFunctionCallRequest);
// var
//   LResult: TFunctionCallResult;
//   LResultJson: string;
// begin
//   // This would typically run in a separate thread or use an async mechanism
//   // For now, simulate by calling the sync version and then the event
//   LResult := ExecuteFunction(Request);
//
//   // Serialize LResult to JSON (needs a proper JSON library)
//   // For now, a very basic placeholder:
//   LResultJson := Format('{"functionName": "%s", "status": "%s", "resultData": %s, "requestId": "%s", "errorMessage": "%s"}',
//     [LResult.FunctionName, LResult.Status, LResult.ResultData, LResult.RequestId, LResult.ErrorMessage]);
//   // The LResult.ResultData is often already JSON, so be careful with double encoding.
//   // A proper serializer would handle TFunctionCallResult -> JSON string.
//
//   if Assigned(FOnOperationCompleted) then
//   begin
//     // Ensure this callback is thread-safe if called from a worker thread
//     // TThread.Synchronize(nil, procedure begin FOnOperationCompleted(Self, LResultJson); end);
//     FOnOperationCompleted(Self, LResultJson);
//   end;
//   LResult.Free; // If ExecuteFunction returns an owned object that isn't managed elsewhere
// end;

initialization
  // Initialization, if any

finalization
  // Finalization, if any

end.
