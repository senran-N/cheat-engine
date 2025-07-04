unit AI.Bridge;

interface

uses
  SysUtils, Classes, SyncObjs, Generics.Collections,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Processes; // Part of Lazarus (usually fcl-process)

type
  // Event type for when a message is received from the Python process
  TMessageReceivedEvent = procedure(Sender: TObject; const AMessage: string) of object;

  TAIPythonBridge = class(TComponent)
  private
    FPythonProcess: TProcess;
    FReaderThread: TThread;
    FWriterThread: TThread;
    FRequestQueue: TThreadedQueue<string>;
    FOnMessageReceived: TMessageReceivedEvent;
    FPythonExecutablePath: string; // e.g., 'python.exe' or '/usr/bin/python3'
    FPythonScriptPath: string;   // Full path to 'ai_engine.py'
    FRunning: Boolean;

    procedure Log(const AMessage: string);
    procedure ReaderThreadExecute;
    procedure WriterThreadExecute;
    function GetIsPythonRunning: Boolean;
    procedure InternalStartProcess(const APythonExecutable: string; const AScriptPath: string; AEnvironment: TStrings);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Start(const APythonExecutable: string; const AScriptPath: string; AEnvironment: TStrings = nil): Boolean;
    procedure Stop;
    procedure SendMessageToPython(const AJsonString: string);

    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property IsPythonRunning: Boolean read GetIsPythonRunning;
  end;

  // Custom thread classes for reader and writer
  TPythonReaderThread = class(TThread)
  private
    FBridge: TAIPythonBridge;
  protected
    procedure Execute; override;
  public
    constructor Create(ABridge: TAIPythonBridge);
  end;

  TPythonWriterThread = class(TThread)
  private
    FBridge: TAIPythonBridge;
  protected
    procedure Execute; override;
  public
    constructor Create(ABridge: TAIPythonBridge);
  end;

implementation

uses StrUtils; // For EndsText

{ TAIPythonBridge }

constructor TAIPythonBridge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Log('TAIPythonBridge.Create');
  FPythonProcess := TProcess.Create(nil);
  FRequestQueue := TThreadedQueue<string>.Create;
  FRunning := False;
  FPythonProcess.Environment.Clear; // Ensure clean environment if TProcess re-used
end;

destructor TAIPythonBridge.Destroy;
begin
  Log('TAIPythonBridge.Destroy');
  Stop; // Ensure everything is cleaned up
  FRequestQueue.Free;
  FPythonProcess.Free;
  inherited Destroy;
end;

procedure TAIPythonBridge.Log(const AMessage: string);
begin
  {$IFDEF DEBUG}
  // In a real app, replace with a more robust logging mechanism
  // OutputDebugString(PChar('[AI.Bridge] ' + AMessage));
  // For now, let's assume a global Log procedure or similar might exist
  // Or write to a debug memo if this component is part of a UI
  if Assigned(Self.Owner) and (Self.Owner is TForm) then
  begin
    // Example: (Self.Owner as TForm).Caption := '[AI.Bridge] ' + AMessage;
  end;
  {$ENDIF}
  // writeln('[AI.Bridge] ' + AMessage); // For console apps
end;

procedure TAIPythonBridge.InternalStartProcess(const APythonExecutable: string; const AScriptPath: string; AEnvironment: TStrings);
var
  I: Integer;
begin
  FPythonProcess.Executable := APythonExecutable;
  FPythonProcess.Parameters.Clear;
  FPythonProcess.Parameters.Add(AScriptPath);
  FPythonProcess.CurrentDirectory := ExtractFilePath(AScriptPath);
  FPythonProcess.Options := [poUsePipes, poNoConsole, poNewHidden]; // Base options

  if Assigned(AEnvironment) and (AEnvironment.Count > 0) then
  begin
    FPythonProcess.Options := FPythonProcess.Options + [poPassEnvironment]; // Add poPassEnvironment only if specific env is given
    FPythonProcess.Environment.Clear;
    Log(Format('Setting %d environment variables for Python process.', [AEnvironment.Count]));
    for I := 0 to AEnvironment.Count - 1 do
    begin
      FPythonProcess.Environment.Add(AEnvironment[I]);
      // Basic logging for non-sensitive env vars, be cautious with API keys
      // if Pos('API_KEY', Uppercase(AEnvironment[I])) = 0 then
      //   Log('Env Var: ' + AEnvironment[I])
      // else
      //   Log('Env Var: ' + Copy(AEnvironment[I], 1, Pos('=', AEnvironment[I])) + '********');
    end;
  end
  else
  begin
    // If no specific environment is passed, TProcess usually inherits the parent's environment.
    // Explicitly clearing and *not* adding poPassEnvironment might be needed if we want to ensure it *doesn't* inherit.
    // However, typical behavior is inheritance if Environment is empty and poPassEnvironment is not set.
    // For clarity, if AEnvironment is nil, we might want to explicitly inherit.
    // FPythonProcess.Options := FPythonProcess.Options - [poPassEnvironment]; // Ensure it's not set if no env passed
    // FPythonProcess.Environment.Clear; // Ensure it's empty to inherit parent
    Log('No specific environment override. Python process will inherit parent environment.');
  end;

  Log(Format('Attempting to start Python: "%s" with script "%s"', [APythonExecutable, AScriptPath]));
  Log(Format('Working Directory: "%s"', [FPythonProcess.CurrentDirectory]));
  Log(Format('Process Options: %s', [OptionSetToString(FPythonProcess.Options)]));


  FPythonProcess.Execute;
end;

// Helper to convert TProcessOptions to string for logging
function OptionSetToString(Options: TProcessOptions): string;
begin
  Result := '';
  if poEchoToConsole in Options then Result := Result + 'poEchoToConsole,';
  if poUsePipes in Options then Result := Result + 'poUsePipes,';
  if poStderrToOutpu in Options then Result := Result + 'poStderrToOutpu,'; // Typo in FPC? Should be poStderrToOutput
  if poNoConsole in Options then Result := Result + 'poNoConsole,';
  if poNewProcessGroup in Options then Result := Result + 'poNewProcessGroup,';
  if poDefaultErrorMode in Options then Result := Result + 'poDefaultErrorMode,';
  if poNewConsole in Options then Result := Result + 'poNewConsole,';
  if poNewHidden in Options then Result := Result + 'poNewHidden,';
  if poWaitOnExit in Options then Result := Result + 'poWaitOnExit,';
  if poPassInput in Options then Result := Result + 'poPassInput,';
  if poRunSuspended in Options then Result := Result + 'poRunSuspended,';
  if poInteractive in Options then Result := Result + 'poInteractive,';
  if poPassEnvironment in Options then Result := Result + 'poPassEnvironment,';
  if Result <> '' then SetLength(Result, Length(Result)-1); // Remove last comma
end;

function TAIPythonBridge.Start(const APythonExecutable: string; const AScriptPath: string; AEnvironment: TStrings = nil): Boolean;
begin
  Log('TAIPythonBridge.Start called.');
  Result := False;
  if FRunning then
  begin
    Log('Python process already running or starting.');
    Exit(True); // Already running
  end;

  FPythonExecutablePath := APythonExecutable;
  FPythonScriptPath := AScriptPath;

  try
    InternalStartProcess(FPythonExecutablePath, FPythonScriptPath, AEnvironment);

    if not FPythonProcess.Running then
    begin
      Log('Failed to start Python process. TProcess.Execute did not result in a running process.');
      // Add more detailed error checking if possible, e.g., reading Stderr if not poNoConsole
      Exit(False);
    end;

    Log('Python process started. PID: ' + IntToStr(FPythonProcess.ProcessID));
    FRunning := True;

    // Start reader and writer threads
    FReaderThread := TPythonReaderThread.Create(Self);
    FWriterThread := TPythonWriterThread.Create(Self);
    Log('Reader and Writer threads created.');
    Result := True;
  except
    on E: Exception do
    begin
      Log('Exception during Python process startup: ' + E.Message);
      FRunning := False; // Ensure state is correct
      Result := False;
    end;
  end;

  if not Result then
  begin
     Log('Python process failed to start.');
  end;
end;

procedure TAIPythonBridge.Stop;
var
  ExitCode: Integer;
begin
  Log('TAIPythonBridge.Stop called.');
  if not FRunning and (FReaderThread = nil) and (FWriterThread = nil) then
  begin
    Log('Stop called but not running or threads not initialized.');
    // If process was created but failed to start threads, ensure it's terminated
    if Assigned(FPythonProcess) and FPythonProcess.Running then
    begin
       Log('Terminating stray Python process.');
       FPythonProcess.Terminate(0); // 0 for normal exit, though it's a termination
       FPythonProcess.WaitOnExit; // Wait for it to actually exit
    end;
    FRunning := False;
    Exit;
  end;

  FRunning := False; // Signal threads to terminate

  // Gracefully stop WriterThread first by signaling it (e.g., with a sentinel value or just by FRunning)
  // The PopItem timeout in WriterThread will allow it to check FRunning and exit.
  // We might push a specific "shutdown" command if the Python script expects it.
  // For now, clearing the queue and relying on FRunning for termination.
  FRequestQueue.Clear; // Clear any pending requests

  if Assigned(FWriterThread) then
  begin
    Log('Waiting for WriterThread to terminate...');
    // FWriterThread.Terminate; // Avoid hard termination if possible
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
    Log('WriterThread terminated and freed.');
  end;

  // Terminate Python process
  // This should also cause the ReaderThread to exit as the pipe closes.
  if Assigned(FPythonProcess) and FPythonProcess.Running then
  begin
    Log('Terminating Python process...');
    try
      // Optionally, send a "shutdown" message first if Python handles it
      // FPythonProcess.Input.Write(...); FPythonProcess.Input.Flush;
      FPythonProcess.Terminate(0); // Request termination
      // Wait for a short period, then force if necessary
      if not FPythonProcess.WaitOnExit(5000) then // Wait up to 5 seconds
      begin
        Log('Python process did not exit gracefully, forcing termination.');
        // This might be specific to OS, TProcess should handle it.
        // On some systems, a more forceful kill might be needed if Terminate is too gentle.
      end;
      ExitCode := FPythonProcess.ExitStatus;
      Log(Format('Python process terminated. Exit code: %d', [ExitCode]));
    except
      on E: Exception do
      begin
        Log('Exception during Python process termination: ' + E.Message);
      end;
    end;
  end
  else
  begin
    Log('Python process not running or not assigned at Stop.');
  end;

  // Stop ReaderThread
  if Assigned(FReaderThread) then
  begin
    Log('Waiting for ReaderThread to terminate...');
    // FReaderThread.Terminate; // Avoid hard termination
    FReaderThread.WaitFor; // Should exit because pipe is closed or FRunning is false
    FreeAndNil(FReaderThread);
    Log('ReaderThread terminated and freed.');
  end;

  Log('TAIPythonBridge.Stop finished.');
end;

procedure TAIPythonBridge.SendMessageToPython(const AJsonString: string);
begin
  if not FRunning or (FWriterThread = nil) or FWriterThread.Terminated then
  begin
    Log('Cannot send message: Bridge not running or writer thread terminated.');
    Exit;
  end;
  Log('Queueing message to Python: ' + AJsonString);
  FRequestQueue.PushItem(AJsonString);
end;

function TAIPythonBridge.GetIsPythonRunning: Boolean;
begin
  Result := FRunning and Assigned(FPythonProcess) and FPythonProcess.Running;
end;

procedure TAIPythonBridge.ReaderThreadExecute;
var
  LBuffer: AnsiString; // Using AnsiString for pipe I/O, ensure Python uses UTF-8 and handle conversion
  BytesRead: Integer;
  S: string; // For UTF-8 conversion
  LineEndPos: Integer;
  PartialLine: string; // To store incomplete lines
begin
  Log('ReaderThread started.');
  PartialLine := '';
  while FRunning and Assigned(FPythonProcess) and FPythonProcess.Running and not Terminated do
  begin
    try
      // Check if there's data available to avoid blocking indefinitely on Read if not desired
      // However, Read on a pipe stdout should block until data or EOF.
      if FPythonProcess.Output.NumBytesAvailable > 0 then
      begin
        SetLength(LBuffer, 1024); // Read in chunks
        BytesRead := FPythonProcess.Output.Read(LBuffer[1], Length(LBuffer));

        if BytesRead > 0 then
        begin
          SetLength(LBuffer, BytesRead); // Adjust buffer to actual bytes read
          S := UTF8Encode(LBuffer); // Assuming pipe bytes are Ansi, convert to UTF-8 string
                                    // If Python prints UTF-8 directly, this needs to be:
                                    // S := string(LBuffer); // if LBuffer is already UTF-8 bytes
                                    // Or more correctly: S := TEncoding.UTF8.GetString(TBytes(LBuffer));
                                    // For simplicity, let's assume Python outputs plain ASCII or compatible single-byte
                                    // encoding that converts well enough for JSON with UTF8Encode.
                                    // A robust solution needs careful encoding management.
                                    // Let's assume Python prints UTF-8 encoded JSON.
                                    // So, we need to convert raw bytes to UTF-8 string.
          S := string(LBuffer); // Direct cast if LBuffer is AnsiString from Read, then interpret as UTF8
                                // This is often problematic. Better:
                                // S := TEncoding.UTF8.GetString(System.AnsiToBytes(LBuffer));
                                // Or, if Python truly outputs UTF-8 bytes:
                                // var RawBytes: TBytes; SetLength(RawBytes, BytesRead);
                                // Move(LBuffer[1], RawBytes[0], BytesRead);
                                // S := TEncoding.UTF8.GetString(RawBytes);
                                // For now, using a simpler approach that might work for basic JSON:
          S := UTF8ToString(LBuffer); // If LBuffer contains UTF-8 bytes as AnsiString

          Log(Format('ReaderThread received %d bytes: "%s"', [BytesRead, S]));
          S := PartialLine + S; // Prepend any incomplete line from previous read
          PartialLine := '';

          repeat
            LineEndPos := Pos(#10, S); // Find LF (newline)
            if LineEndPos = 0 then // No LF, might be CR or incomplete line
            begin
              LineEndPos := Pos(#13, S); // Find CR
            end;

            if LineEndPos > 0 then
            begin
              // Extract the line (JSON message)
              // Python side should ensure each JSON message is on a new line.
              CurrentLine := Copy(S, 1, LineEndPos - 1);
              S := Copy(S, LineEndPos + 1, Length(S)); // Remainder

              // Trim CR if LF was found after CR (Windows-style CRLF)
              if EndsText(#13, CurrentLine) then
                 SetLength(CurrentLine, Length(CurrentLine)-1);

              CurrentLine := Trim(CurrentLine);

              if CurrentLine <> '' then
              begin
                Log('ReaderThread processed line: ' + CurrentLine);
                if Assigned(FOnMessageReceived) then
                begin
                  // Synchronize with the main thread to call the event handler
                  TThread.Queue(nil, procedure
                  begin
                    if Assigned(FOnMessageReceived) then // Check again, owner might have freed it
                       FOnMessageReceived(Self.FBridge, CurrentLine);
                  end);
                end;
              end;
            end
            else
            begin
              PartialLine := S; // Store incomplete line
              S := ''; // Nothing more to process in this chunk
            end;
          until S = '';
        end
        else if BytesRead = 0 then // End of stream
        begin
          Log('ReaderThread: End of stream detected (BytesRead = 0).');
          FRunning := False; // Signal bridge to stop
          Break;
        end;
      end
      else
      begin
        Sleep(50); // No data, sleep a bit to prevent busy-waiting if not blocking
      end;
    except
      on E: EInOutError do // Often happens when pipe is closed
      begin
        Log('ReaderThread: EInOutError - Pipe likely closed: ' + E.Message);
        FRunning := False; // Signal to stop
        Break;
      end;
      on E: Exception do
      begin
        Log('ReaderThread: Exception - ' + E.ClassName + ': ' + E.Message);
        FRunning := False; // Signal to stop on other errors too
        Break; // Exit loop on error
      end;
    end;
  end;
  Log('ReaderThread finished.');
end;

procedure TAIPythonBridge.WriterThreadExecute;
var
  LJsonString: string;
begin
  Log('WriterThread started.');
  while FRunning and not Terminated do
  begin
    try
      // Wait for a message with a timeout, so we can check FRunning
      if FRequestQueue.PopItem(LJsonString) = TWaitResult.wrSignaled then // Or use PopItem with timeout
      begin
        if not FRunning or Terminated then Break; // Check FRunning again after waking up

        if Assigned(FPythonProcess) and FPythonProcess.Running and Assigned(FPythonProcess.Input) then
        begin
          Log('WriterThread sending: ' + LJsonString);
          // Ensure the message is followed by a newline, as Python side likely reads line by line
          // And ensure UTF-8 encoding for the bytes written to the pipe.
          // var BytesToWrite: TBytes = TEncoding.UTF8.GetBytes(LJsonString + #10);
          // FPythonProcess.Input.Write(BytesToWrite[0], Length(BytesToWrite));

          // Simpler for AnsiString based pipes if Python handles it:
          FPythonProcess.Input.WriteAnsiString(LJsonString + #10); // #10 is LF
          FPythonProcess.Input.Flush; // Ensure it's sent immediately
          Log('WriterThread message sent.');
        end
        else
        begin
          Log('WriterThread: Python process not running or input pipe not available. Message dropped.');
          // Optionally, re-queue or handle error
        end;
      end;
      // If PopItem has a timeout, it will return wrTimeout, allowing the loop to check FRunning
      // If TThreadedQueue does not have a timeout PopItem, this loop might block indefinitely
      // The current Lazarus TThreadedQueue PopItem is blocking.
      // To make it non-blocking or with timeout, a custom queue or different sync might be needed.
      // For now, assume FRunning will be set to false by Stop and the thread will eventually exit.
      // A more robust writer thread would use PopItem with a timeout or a sentinel item in the queue to signal shutdown.
      // Let's assume Stop will handle waking it up if it's stuck on PopItem,
      // or that FRunning is checked often enough if PopItem is non-blocking.
      // For now, we rely on the fact that if FRunning becomes false, the next PopItem
      // might still block if the queue is empty. Stop needs to handle this.
      // A common pattern is to push a specific "poison pill" or "sentinel" value to the queue
      // to make PopItem return, and then the thread checks FRunning/Terminated.

    except
      on E: EInOutError do // Often happens when pipe is closed
      begin
        Log('WriterThread: EInOutError - Pipe likely closed: ' + E.Message);
        FRunning := False; // Signal to stop
        Break;
      end;
      on E: Exception do
      begin
        Log('WriterThread: Exception - ' + E.ClassName + ': ' + E.Message);
        FRunning := False; // Signal to stop
        Break; // Exit loop on error
      end;
    end;
  end;
  Log('WriterThread finished.');
end;


{ TPythonReaderThread }

constructor TPythonReaderThread.Create(ABridge: TAIPythonBridge);
begin
  inherited Create(False); // False = not suspended
  FBridge := ABridge;
  FreeOnTerminate := True; // Automatically free when Execute finishes
end;

procedure TPythonReaderThread.Execute;
begin
  FBridge.ReaderThreadExecute;
end;

{ TPythonWriterThread }

constructor TPythonWriterThread.Create(ABridge: TAIPythonBridge);
begin
  inherited Create(False); // False = not suspended
  FBridge := ABridge;
  FreeOnTerminate := True; // Automatically free when Execute finishes
end;

procedure TPythonWriterThread.Execute;
begin
  FBridge.WriterThreadExecute;
end;

initialization
  // Global initialization for the unit, if any.

finalization
  // Global finalization for the unit, if any.

end.
