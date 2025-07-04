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
  SyncObjs; // For TCriticalSection, if using threads directly for scans

type
  TScanTaskStatus = (stsPending, stsRunning, stsCompleted, stsError, stsCancelled);

  TScanTask = class
  public
    TaskId: string;
    Status: TScanTaskStatus;
    Progress: Double; // 0.0 to 1.0
    FoundCount: Integer;
    Results: TStringList; // List of addresses as strings
    ErrorMessage: string;
    ScanValue: string;
    ScanValue2: string; // For "between" scans
    ScanType: string;
    ScanOption: string;
    // Add other parameters like start_address, end_address if implemented
    CreationTime: TDateTime;
    LastAccessTime: TDateTime;

    // For actual threading, a TThread descendant would be better
    // For simulation, we'll just update status fields.
    // ScanThread: TThread; // Placeholder

    constructor Create(AId: string; AValue, AValue2, AType, AOption: string);
    destructor Destroy; override;
    procedure SimulateProgress; // Simulates scan progress
  end;


{ TAIOperationExecutor }
  TAIOperationExecutor = class
  private
    FOnOperationCompleted: TAsyncOperationCompletedEvent;
    FActiveScanTasks: TObjectDictionary<string, TScanTask>;
    FScanTaskLock: TCriticalSection; // To protect FActiveScanTasks
    FNextTaskId: Integer;

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

  public
    constructor Create;
    destructor Destroy; override;
begin // TAIOperationExecutor.Create
  inherited Create;
  FActiveScanTasks := TObjectDictionary<string, TScanTask>.Create([doOwnsValues]);
  FScanTaskLock := TCriticalSection.Create;
  FNextTaskId := 0;
  LogRequest('TAIOperationExecutor Created.');
end;

destructor TAIOperationExecutor.Destroy;
begin
  LogRequest('TAIOperationExecutor Destroying...');
  FScanTaskLock.Enter;
  try
    // Clearing the dictionary will free TScanTask objects because of doOwnsValues
    FActiveScanTasks.Clear;
  finally
    FScanTaskLock.Leave;
  end;
  FreeAndNil(FActiveScanTasks);
  FreeAndNil(FScanTaskLock);
  LogRequest('TAIOperationExecutor Destroyed.');
  inherited Destroy;
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
constructor TScanTask.Create(AId: string; AValue, AValue2, AType, AOption: string);
begin
  inherited Create;
  TaskId := AId;
  ScanValue := AValue;
  ScanValue2 := AValue2;
  ScanType := AType;
  ScanOption := AOption;
  Status := stsPending;
  Progress := 0.0;
  FoundCount := 0;
  Results := TStringList.Create;
  ErrorMessage := '';
  CreationTime := Now;
  LastAccessTime := CreationTime;
  // In a real implementation, start a thread here or add to a queue for a worker thread pool
  // For simulation purposes, we'll change status to running almost immediately.
  // A proper implementation would likely involve a TThread descendant that performs the scan.
  // The TThread would update these TScanTask fields.
  Status := stsRunning; // Simulate it starts running
  LogRequest(Format('Scan task %s created and set to running. Value: "%s", Type: "%s"', [TaskId, ScanValue, ScanType]));
end;

destructor TScanTask.Destroy;
begin
  LogRequest(Format('Scan task %s being destroyed.', [TaskId]));
  Results.Free;
  inherited Destroy;
end;

procedure TScanTask.SimulateProgress;
var
  I: Integer;
  OldStatus: TScanTaskStatus;
begin
  OldStatus := Status;
  if Status = stsRunning then
  begin
    LastAccessTime := Now;
    Progress := Min(1.0, Progress + 0.1 + Random * 0.2); // Random progress, ensure it doesn't exceed 1.0 easily before check

    if Progress >= 1.0 then
    begin
      Progress := 1.0;
      Status := stsCompleted;
      FoundCount := Random(25) + 1; // Simulate 1 to 25 results for brevity
      Results.Clear;
      for I := 0 to FoundCount - 1 do
      begin
        Results.Add(Format('0x%8.8X', [Random($FFFFFFFF)]));
      end;
      LogRequest(Format('Scan task %s SIMULATED completion. Found %d results.', [TaskId, FoundCount]));
    end
    else
    begin
      // LogRequest(Format('Scan task %s progress: %.2f', [TaskId, Progress]));
    end;

    // Simulate an error occasionally for testing, only if not already completed by progress
    if (Status = stsRunning) and (Random < 0.05) then // 5% chance of error during running
    begin
        Status := stsError;
        ErrorMessage := 'Simulated random scan error occurred during scan progress.';
        FoundCount := 0;
        Results.Clear;
        Progress := 1.0; // Errors usually mean scan stops, so progress might be considered complete in a way
        LogRequest(Format('Scan task %s SIMULATED an error.', [TaskId]));
    end;
  end;

  // If status changed, log it
  if OldStatus <> Status then
     LogRequest(Format('Scan task %s status changed from %s to %s.', [TaskId, GetEnumName(TypeInfo(TScanTaskStatus), Ord(OldStatus)), GetEnumName(TypeInfo(TScanTaskStatus), Ord(Status))]));
end;


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
    // Not considered sensitive, no confirmation needed.
    LogRequest('Executing listProcesses...');
    {$IFDEF MSWINDOWS}
    ProcessesArrayJson := TJSONArray.Create;
    try
      SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if SnapshotHandle = INVALID_HANDLE_VALUE then
      begin
        LStatus := 'error';
        LErrorMessage := 'Failed to create snapshot of processes. Error code: ' + IntToStr(GetLastError);
      end
      else
      try
        ProcessEntry.dwSize := SizeOf(TProcessEntry32);
        if Process32First(SnapshotHandle, ProcessEntry) then
        begin
          repeat
            ProcessJsonObj := TJSONObject.Create;
            ProcessJsonObj.Add('pid', ProcessEntry.th32ProcessID);
            ProcessJsonObj.Add('name', string(ProcessEntry.szExeFile)); // string cast for PChar
            ProcessesArrayJson.Add(ProcessJsonObj); // Add object to array, array takes ownership
          until not Process32Next(SnapshotHandle, ProcessEntry);
        end
        else
        begin
          LStatus := 'error';
          LErrorMessage := 'Failed to get first process. Error code: ' + IntToStr(GetLastError);
        end;
      finally
        CloseHandle(SnapshotHandle);
      end;

      if LStatus = 'success' then
      begin
        TempJsonObj := TJSONObject.Create; // Wrapper object: {"processes": [...]}
        TempJsonObj.Add('processes', ProcessesArrayJson); // ProcessesArrayJson is now owned by TempJsonObj
        LResultData := TempJsonObj.AsJSON; // Get the JSON string
        TempJsonObj.Free; // Free the wrapper. It frees ProcessesArrayJson.
      end
      else // Error occurred
      begin
        ProcessesArrayJson.Free; // Array was created but not used or passed to TempJsonObj
        LResultData := '{"processes": []}'; // Ensure valid JSON for error case
      end;
    except
      on E: Exception do
      begin
        LStatus := 'error';
        LErrorMessage := 'Exception during listProcesses: ' + E.Message;
        FreeAndNil(ProcessesArrayJson); // Ensure cleanup on exception
        LResultData := '{"processes": []}';
      end;
    end;
    {$ELSE}
    LStatus := 'error';
    LErrorMessage := 'listProcesses is only implemented for Windows in this stub.';
    LResultData := '{"processes": []}';
    {$ENDIF}
  end
  else if CompareText(LFunctionName, 'openProcess') = 0 then
  begin
    LogRequest('Executing openProcess (sensitive operation)...');
    PID := GetParamValueAsInteger(Request, 'pid', -1, ConversionOk);

    if not ConversionOk then
    begin
      LStatus := 'error';
      LErrorMessage := 'Invalid "pid" parameter: value is not a valid integer.';
    end
    else if PID < 0 then // Assuming PIDs are non-negative. -1 might mean parameter was missing or invalid.
    begin
      LStatus := 'error';
      LErrorMessage := 'Missing or invalid "pid" parameter for openProcess. Must be a non-negative integer.';
    end
    else
    begin
      // User Confirmation
      if not RequestUserConfirmation('Open Process', Format('The AI assistant wants to open process with PID: %d. Do you allow this?', [PID])) then
      begin
        LStatus := 'error'; // Or perhaps a specific status like 'cancelled_by_user'
        LErrorMessage := 'Operation cancelled by user.';
        LResultData := '{"message": "User cancelled opening process."}';
      end
      else
      begin
        // User confirmed, proceed with actual logic
        LogRequest(Format('User confirmed. Attempting to open process with PID: %d', [PID]));

        // Simulate CE API call
        LogRequest(Format('Simulating Cheat Engine attaching to PID: %d', [PID]));
        Dim SimProcessName: string;
        Dim SimSuccess: Boolean = True;

        if PID = 0 then
        begin
            SimSuccess := False;
            LErrorMessage := 'Cannot open process with PID 0.';
            SimProcessName := 'N/A';
        end
        else if PID = 99999 then
        begin
            SimSuccess := False;
            LErrorMessage := Format('Failed to open process %d. It might be protected or not exist.', [PID]);
            SimProcessName := 'N/A';
        end
        else
        begin
            if PID = 1234 then SimProcessName := 'game.exe'
            else if PID = 5678 then SimProcessName := 'client.bin'
            else SimProcessName := Format('Process_%d.exe', [PID]);
        end;

        if SimSuccess then
        begin
          LStatus := 'success';
          LResultData := Format('{"message": "Successfully attached to process %s (PID: %d)"}', [SimProcessName, PID]);
        end
        else
        begin
          LStatus := 'error';
          LResultData := Format('{"message": "Failed to attach to process PID %d. %s"}', [PID, LErrorMessage]);
        end;
      end;
    end;
  end
  else if CompareText(LFunctionName, 'getAttachedProcess') = 0 then
  begin
    LogRequest('Executing getAttachedProcess...');
    // Placeholder for actual GetAttachedProcess logic
    // if CESharedAPIs.IsProcessOpen then
    //   LResultData := Format('{"process": {"pid": %d, "name": "%s"}}', [CESharedAPIs.GetOpenedProcessID, CESharedAPIs.GetOpenedProcessName]);
    // else
    //   LResultData := '{"process": null}';
    LResultData := '{"process": {"pid": 0, "name": "No process attached (stub)"}}';
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

procedure TAIOperationExecutor.ExecuteStartFirstScan(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  ScanValue, ScanValue2, ScanType, ScanOption, TaskId: string;
  NewTask: TScanTask;
  JsonObj: TJSONObject;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';

  ScanValue := GetParamValue(Request, 'value');
  ScanType := GetParamValue(Request, 'type');
  ScanOption := GetParamValue(Request, 'scan_option');
  ScanValue2 := GetParamValue(Request, 'value2', ''); // Optional

  // Basic validation
  if (ScanValue = '') or (ScanType = '') or (ScanOption = '') then
  begin
    LStatus := 'error';
    LErrorMessage := 'Missing required parameters for startFirstScan (value, type, scan_option).';
    LogRequest(LErrorMessage);
    Exit;
  end;

  // User Confirmation (Scans can be intensive)
  if not RequestUserConfirmation('Start Memory Scan', Format('AI wants to start a new memory scan for "%s" (Type: %s, Option: %s). This might take some time. Allow?', [ScanValue, ScanType, ScanOption])) then
  begin
    LStatus := 'error';
    LErrorMessage := 'Scan operation cancelled by user.';
    LResultData := '{"message": "User cancelled the scan operation."}';
    Exit;
  end;

  TaskId := GenerateTaskId;
  NewTask := TScanTask.Create(TaskId, ScanValue, ScanValue2, ScanType, ScanOption);

  FScanTaskLock.Enter;
  try
    FActiveScanTasks.Add(TaskId, NewTask); // Dictionary takes ownership
  finally
    FScanTaskLock.Leave;
  end;

  JsonObj := TJSONObject.Create;
  try
    JsonObj.Add('task_id', TaskId);
    LResultData := JsonObj.AsJSON;
  finally
    JsonObj.Free;
  end;
  LogRequest(Format('Scan task %s started for value "%s", type "%s".', [TaskId, ScanValue, ScanType]));
end;

procedure TAIOperationExecutor.ExecuteGetScanStatus(Request: TFunctionCallRequest; out LResultData: string; out LStatus: string; out LErrorMessage: string);
var
  TaskId: string;
  Task: TScanTask;
  JsonObj: TJSONObject;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';
  TaskId := GetParamValue(Request, 'task_id');

  if TaskId = '' then
  begin
    LStatus := 'error';
    LErrorMessage := 'Missing task_id parameter for getScanStatus.';
    LogRequest(LErrorMessage);
    Exit;
  end;

  FScanTaskLock.Enter;
  try
    if not FActiveScanTasks.TryGetValue(TaskId, Task) then
    begin
      LStatus := 'error';
      LErrorMessage := Format('Scan task with ID "%s" not found.', [TaskId]);
      LogRequest(LErrorMessage);
      Exit; // Exit from try-finally block, lock will be released
    end;

    // Simulate scan progress if it's running
    if Assigned(Task) and (Task.Status = stsRunning) then
      Task.SimulateProgress; // Update progress and possibly status

    JsonObj := TJSONObject.Create;
    try
      case Task.Status of
        stsPending:   JsonObj.Add('status', 'pending');
        stsRunning:
          begin
            JsonObj.Add('status', 'running');
            JsonObj.Add('progress', Task.Progress);
          end;
        stsCompleted:
          begin
            JsonObj.Add('status', 'completed');
            JsonObj.Add('found_count', Task.FoundCount);
          end;
        stsError:
          begin
            JsonObj.Add('status', 'error');
            JsonObj.Add('error_message', Task.ErrorMessage);
          end;
        stsCancelled: JsonObj.Add('status', 'cancelled');
      end;
      LResultData := JsonObj.AsJSON;
      Task.LastAccessTime := Now; // Update last access time
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
  Offset, Count, I, ActualCount: Integer;
  ConversionOk: Boolean;
  JsonObj: TJSONObject;
  ResultsArrayJson: TJSONArray;
begin
  LStatus := 'success';
  LErrorMessage := '';
  LResultData := '{}';
  TaskId := GetParamValue(Request, 'task_id');

  Offset := GetParamValueAsInteger(Request, 'offset', 0, ConversionOk);
  if not ConversionOk then Offset := 0;
  Count := GetParamValueAsInteger(Request, 'count', 20, ConversionOk); // Default to 20 results
  if not ConversionOk then Count := 20;
  if Count <= 0 then Count := 20; // Ensure positive count
  if Count > 100 then Count := 100; // Max 100 results at a time for this example

  if TaskId = '' then
  begin
    LStatus := 'error';
    LErrorMessage := 'Missing task_id parameter for getScanResults.';
    LogRequest(LErrorMessage);
    Exit;
  end;

  FScanTaskLock.Enter;
  try
    if not FActiveScanTasks.TryGetValue(TaskId, Task) then
    begin
      LStatus := 'error';
      LErrorMessage := Format('Scan task with ID "%s" not found.', [TaskId]);
      LogRequest(LErrorMessage);
      Exit;
    end;

    if Task.Status <> stsCompleted then
    begin
      LStatus := 'error';
      LErrorMessage := Format('Scan task "%s" is not completed. Current status: %s.', [TaskId, GetEnumName(TypeInfo(TScanTaskStatus), Ord(Task.Status))]);
      LogRequest(LErrorMessage);
      Exit;
    end;

    JsonObj := TJSONObject.Create;
    try
      ResultsArrayJson := TJSONArray.Create;
      ActualCount := 0;
      for I := Offset to Task.Results.Count - 1 do
      begin
        if ActualCount >= Count then Break; // Got enough results
        ResultsArrayJson.Add(Task.Results[I]);
        Inc(ActualCount);
      end;

      JsonObj.Add('results', ResultsArrayJson); // Array takes ownership
      JsonObj.Add('total_found', Task.FoundCount);
      JsonObj.Add('offset_returned', Offset);
      JsonObj.Add('count_returned', ActualCount);
      LResultData := JsonObj.AsJSON;
      Task.LastAccessTime := Now;
    finally
      JsonObj.Free;
    end;
  finally
    FScanTaskLock.Leave;
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
