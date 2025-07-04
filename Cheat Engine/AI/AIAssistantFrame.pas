unit AIAssistantFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  AI.Bridge, AI.SharedTypes, AI.Executor; // Assuming AI.Core is in search path

type

  { TAIAssistantFrame }

  TAIAssistantFrame = class(TFrame)
    edtInput: TEdit;
    btnSend: TButton;
    memConversation: TRichEdit; // Using TRichEdit for potential future formatting
    pnlBottom: TPanel;
    lblStatus: TLabel;
    procedure btnSendClick(Sender: TObject);
    procedure FrameCreate(Sender: TObject);
    procedure FrameDestroy(Sender: TObject);
  private
    FPythonBridge: TAIPythonBridge;
    FAIOperationExecutor: TAIOperationExecutor;
    procedure LogToConversation(const AText: string; AColor: TColor = clWindowText);
    procedure HandleBridgeMessage(Sender: TObject; const AMessage: string);
    procedure InitializeAIComponents;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupAI(const APythonExecutable: string; const AScriptPath: string);
  end;

implementation

{$R *.lfm} // Lazarus will look for AIAssistantFrame.lfm

uses
  // For JSON parsing, a proper library is needed. For now, very basic string checks.
  // Consider LazUtils.Json, fpJSON, or others.
  LazFileUtils, // For GetAppConfigDir
  AI.Config;    // Added AI Configuration unit

{ TAIAssistantFrame }

constructor TAIAssistantFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Components will be created by LFM loading.
  // If not using LFM, create them here:
  // InitializeComponent; // A private method to create and setup components
end;

destructor TAIAssistantFrame.Destroy;
begin
  // FPythonBridge and FAIOperationExecutor are cleaned up in FrameDestroy
  // if they were created in FrameCreate.
  inherited Destroy;
end;

// If not using LFM, you would have a method like this:
(*
procedure TAIAssistantFrame.InitializeComponent;
begin
  // This is a conceptual representation if LFM is not used or for understanding.
  // Lazarus IDE manages this via the LFM.

  Self.Width := 400;
  Self.Height := 300;

  memConversation := TRichEdit.Create(Self);
  memConversation.Parent := Self;
  memConversation.Align := alClient;
  memConversation.ReadOnly := True;
  memConversation.ScrollBars := ssVertical;
  memConversation.Font.Name := 'Courier New'; // Monospaced font is good for logs

  pnlBottom := TPanel.Create(Self);
  pnlBottom.Parent := Self;
  pnlBottom.Align := alBottom;
  pnlBottom.Height := 70;
  pnlBottom.BevelOuter := bvNone;

  lblStatus := TLabel.Create(Self);
  lblStatus.Parent := pnlBottom;
  lblStatus.Align := alTop;
  lblStatus.Caption := 'Status: Idle';
  lblStatus.AutoSize := False;
  lblStatus.Height := 20;

  edtInput := TEdit.Create(Self);
  edtInput.Parent := pnlBottom;
  edtInput.Align := alClient; // Will fill space between status and button
  edtInput.Top := lblStatus.Top + lblStatus.Height + 5;
  edtInput.Height := 25;
  edtInput.Anchors := [akLeft, akRight, akBottom]; // Adjust as panel resizes

  btnSend := TButton.Create(Self);
  btnSend.Parent := pnlBottom;
  btnSend.Align := alRight;
  btnSend.Caption := 'Send';
  btnSend.OnClick := @btnSendClick;
  btnSend.Top := edtInput.Top; // Align with edit
  btnSend.Width := 75;
  btnSend.Height := edtInput.Height;
  btnSend.Anchors := [akRight, akBottom];

  // Adjust edtInput width considering the button
  edtInput.Width := pnlBottom.ClientWidth - btnSend.Width - 10; // Some margin
end;
*)

procedure TAIAssistantFrame.FrameCreate(Sender: TObject);
begin
  // This is where components created by LFM are available.
  // Initialize AI related components
  InitializeAIComponents;

  // Example: Set Python path (this should come from configuration)
  // For testing, we can hardcode paths or use relative paths.
  // The actual paths will be passed via SetupAI method.
  LogToConversation('AI Assistant Frame Created. Call SetupAI to start.', clGray);
end;

procedure TAIAssistantFrame.InitializeAIComponents;
var
  LConfig: TAIConfig;
begin
  LogToConversation('Initializing AI Components...', clGray);
  try
    // Load configuration
    AIConfigManager.LoadConfig; // Ensure it's loaded or defaults created
    LConfig := AIConfigManager.GetConfig;
    LogToConversation('AI Configuration loaded.', clGray);
    LogToConversation(Format('Python Path: %s', [LConfig.PythonExecutablePath]), clGray);
    LogToConversation(Format('OpenAI Model: %s, BaseURL: %s', [LConfig.ModelId, LConfig.APIBaseUrl]), clGray);
    if LConfig.APIKey = '' then
      LogToConversation('OpenAI API Key is NOT set in config.', clOrange)
    else
      LogToConversation('OpenAI API Key is set in config.', clGray);


    FAIOperationExecutor := TAIOperationExecutor.Create;
    LogToConversation('AI Operation Executor created.', clGray);

    FPythonBridge := TAIPythonBridge.Create(Self); // Self as owner for cleanup
    FPythonBridge.OnMessageReceived := @HandleBridgeMessage;
    LogToConversation('Python Bridge created.', clGray);

    // Automatically call SetupAI with loaded config
    // The AScriptPath needs to be determined, perhaps relative to CE or AI module path
    // For now, assuming a fixed relative path for ai_engine.py from the main app executable.
    Dim PyScriptPath: String = 'AI_Python' + PathDelim + 'ai_engine.py';
    SetupAI(LConfig.PythonExecutablePath, PyScriptPath, LConfig);

  except
    on E: Exception do
    begin
      LogToConversation('Error initializing AI components: ' + E.Message, clRed);
      ShowMessage('Error initializing AI components: ' + E.Message);
    end;
  end;
end;

procedure TAIAssistantFrame.SetupAI(const APythonExecutable: string; const AScriptPath: string; const AConfig: TAIConfig);
var
  ScriptFullPath: string;
  EnvStrings: TStringList;
  I: Integer;
begin
  if not Assigned(FPythonBridge) then
  begin
    LogToConversation('Python Bridge not initialized. Cannot start AI.', clRed);
    Exit;
  end;

  LogToConversation(Format('Attempting to start AI with Python: "%s", Script: "%s"', [APythonExecutable, AScriptPath]), clGray);

  // Ensure script path is absolute or resolve it relative to application
  // If AScriptPath is already relative to the application executable (e.g. "AI_Python/ai_engine.py")
  ScriptFullPath := ExpandFileName(ExtractFilePath(ParamStr(0)) + AScriptPath);

  if not FileExists(ScriptFullPath) then
  begin
      // Try relative to the current directory as a fallback (e.g. if running from IDE with different workdir)
      ScriptFullPath := ExpandFileName(AScriptPath);
      if not FileExists(ScriptFullPath) then
      begin
        LogToConversation(Format('AI Script not found at: "%s" (tried relative to exe and current dir)', [AScriptPath]), clRed);
        lblStatus.Caption := 'Status: Error - Script not found';
        Exit;
      end;
  end;
  LogToConversation(Format('Script full path resolved to: "%s"', [ScriptFullPath]), clGray);

  // Prepare environment variables for the Python process
  EnvStrings := TStringList.Create;
  try
    // Inherit existing environment
    for I := 0 to GetEnvironmentVariableCount - 1 do
      EnvStrings.Add(GetEnvironmentString(I));

    // Set/Override OpenAI specific environment variables from TAIConfig
    EnvStrings.Values['OPENAI_API_KEY'] := AConfig.APIKey;
    EnvStrings.Values['OPENAI_BASE_URL'] := AConfig.APIBaseUrl;
    EnvStrings.Values['OPENAI_MODEL_ID'] := AConfig.ModelId;
    // Python script can read these via os.environ.get()
    // Temperature & MaxTokens are typically passed in API calls, not env vars,
    // but Python script could be modified to read them from env if desired for defaults.
    // For now, Python script uses its own defaults for these.

    LogToConversation('Setting environment for Python: OPENAI_MODEL_ID, OPENAI_BASE_URL set.', clGray);
    if AConfig.APIKey <> '' then
        LogToConversation('OPENAI_API_KEY will be passed to Python environment.', clGray)
    else
        LogToConversation('Warning: OPENAI_API_KEY is empty, Python AI will likely fail to init OpenAI.', clOrange);


    if FPythonBridge.Start(APythonExecutable, ScriptFullPath, EnvStrings) then
    begin
      LogToConversation('Python Bridge started successfully.', clGreen);
      lblStatus.Caption := 'Status: AI Python process running.';

      // Send system_init message with function definitions
      LogToConversation('Sending function definitions to AI Engine...', clGray);
      Dim FuncSchemasJsonString: string = AIFunctions.GetAllFunctionsSchemaJson();
      Dim SystemInitJsonObj: TJSONObject;
      Dim ToolsJsonArray: TJSONArray;
      Dim SystemInitMessageString: string;

      try
        // Parse the schemas string into a TJSONArray to ensure it's valid and to embed it correctly
        ToolsJsonArray := TJSONParser.Create(FuncSchemasJsonString).Parse as TJSONArray; // Ownership by parser then by SystemInitJsonObj

        SystemInitJsonObj := TJSONObject.Create;
        SystemInitJsonObj.Add('message_type', 'system_init');
        SystemInitJsonObj.Add('tools', ToolsJsonArray); // ToolsJsonArray is now owned by SystemInitJsonObj

        SystemInitMessageString := SystemInitJsonObj.AsJSON; // Compact JSON
        FPythonBridge.SendMessageToPython(SystemInitMessageString);
        LogToConversation('Function definitions sent to AI Engine.', clGreen);
        lblStatus.Caption := 'Status: AI Ready';
      except
        on E: Exception do
        begin
          LogToConversation('Error preparing or sending function definitions: ' + E.Message, clRed);
          lblStatus.Caption := 'Status: Error - AI Init Failed (tools)';
        end;
        // SystemInitJsonObj might not be assigned or ToolsJsonArray might not if error before creation
        // If SystemInitJsonObj was created, it needs freeing if not passed to SendMessageToPython
        // However, AsJSON is called before SendMessageToPython, so SystemInitJsonObj should be valid if no exception.
        // If an exception occurs during Add or AsJSON, fpJSON objects might need manual cleanup if not fully constructed.
        // TJSONObject.Create and then Add(TJSONData) makes the parent own the child.
        // Freeing the root (SystemInitJsonObj) would free children.
        // If parsing ToolsJsonArray fails, it won't be added.
        // For simplicity here, assuming SystemInitJsonObj.Free in a finally block if it was assigned.
        // More robustly:
        if Assigned(SystemInitJsonObj) then SystemInitJsonObj.Free; // Free if created and an error occurred before sending.
                                                                  // Note: if Add('tools', ToolsJsonArray) succeeded, ToolsJsonArray is owned.
                                                                  // If parsing FuncSchemasJsonString failed, ToolsJsonArray is not assigned.
      end;
  end
  else
  begin
    LogToConversation('Failed to start Python Bridge. Check logs or Python setup.', clRed);
    lblStatus.Caption := 'Status: Error - AI Failed to start';
  end;
end;

procedure TAIAssistantFrame.FrameDestroy(Sender: TObject);
begin
  LogToConversation('AI Assistant Frame Destroying...', clGray);
  if Assigned(FPythonBridge) then
  begin
    LogToConversation('Stopping Python Bridge...', clGray);
    FPythonBridge.Stop;
    FreeAndNil(FPythonBridge); // Bridge created with Self as owner, but explicit free is fine
    LogToConversation('Python Bridge stopped and freed.', clGray);
  end;

  if Assigned(FAIOperationExecutor) then
  begin
    FreeAndNil(FAIOperationExecutor);
    LogToConversation('AI Operation Executor freed.', clGray);
  end;
end;

procedure TAIAssistantFrame.btnSendClick(Sender: TObject);
var
  UserInput: string;
  UserMessageJson: string;
  // Temp: Create a TUserMessage for serialization (if a JSON lib was fully integrated)
  // Msg: TUserMessage;
begin
  UserInput := Trim(edtInput.Text);
  if UserInput = '' then
    Exit;

  LogToConversation('You: ' + UserInput, clBlue);

  if not Assigned(FPythonBridge) or not FPythonBridge.IsPythonRunning then
  begin
    LogToConversation('AI is not running. Cannot send message.', clRed);
    lblStatus.Caption := 'Status: AI Not Running';
    Exit;
  end;

  // Construct JSON using TAIJsonUtils
  Dim UserMsg: TUserMessage;
  UserMsg := TUserMessage.Create(UserInput);
  try
    UserMessageJson := TAIJsonUtils.MessageToJsonString(UserMsg);
  finally
    UserMsg.Free;
  end;

  FPythonBridge.SendMessageToPython(UserMessageJson);
  edtInput.Clear;
  lblStatus.Caption := 'Status: Sending to AI...';
end;

procedure TAIAssistantFrame.LogToConversation(const AText: string; AColor: TColor = clWindowText);
begin
  if Assigned(memConversation) then
  begin
    memConversation.SelStart := Length(memConversation.Text);
    memConversation.SelAttributes.Color := AColor;
    memConversation.SelText := AText + sLineBreak;
    memConversation.SelStart := Length(memConversation.Text);
    // memConversation.ScrollToCaret; // May not exist, TRichEdit scrolls automatically
  end;
end;

procedure TAIAssistantFrame.HandleBridgeMessage(Sender: TObject; const AMessage: string);
var
  // Placeholder for JSON parsing. We need a robust JSON library.
  // For now, simple string checks for message_type.
  MsgType: string;
  Content: string;
  FunctionName: string;
  RequestId: string;
  // JsonDoc: TJSONData; // Example with fpJSON
  CallResult: TFunctionCallResult;
  AIResponse: TAIResponse;
  UserMsg: TUserMessage; // just for type check, not used further here
  ErrorMsg: TErrorMessage;
  FuncCallReq: TFunctionCallRequest;

begin
  LogToConversation('AI Raw: ' + AMessage, clOlive);
  lblStatus.Caption := 'Status: AI Responded';

  Dim BaseMsg: TBaseMessage = nil;
  Dim ResultJson: string;

  if TAIJsonUtils.JsonStringToBaseMessage(AMessage, BaseMsg) and Assigned(BaseMsg) then
  try
    if BaseMsg is TAIResponse then
    begin
      AIResponse := TAIResponse(BaseMsg);
      LogToConversation('AI: ' + AIResponse.Content, clNavy);
      lblStatus.Caption := 'Status: AI Ready';
    end
    else if BaseMsg is TFunctionCallRequest then
    begin
      FuncCallReq := TFunctionCallRequest(BaseMsg);
      LogToConversation(Format('AI requests function call: %s (ID: %s)', [FuncCallReq.FunctionName, FuncCallReq.RequestId]), clPurple);
      // Log parameters
      if FuncCallReq.Parameters.Count > 0 then
      begin
        Dim I: Integer;
        Dim ParamDetails: string = 'Params: ';
        for I := 0 to FuncCallReq.Parameters.Count - 1 do
        begin
          Dim Param: TFunctionCallParameter = FuncCallReq.Parameters[I] as TFunctionCallParameter;
          ParamDetails := ParamDetails + Format('%s="%s"; ', [Param.Name, Param.Value]);
        end;
        LogToConversation(ParamDetails, clPurple);
      end;

      lblStatus.Caption := Format('Status: AI executing %s...', [FuncCallReq.FunctionName]);

      if Assigned(FAIOperationExecutor) then
      begin
        CallResult := FAIOperationExecutor.ExecuteFunction(FuncCallReq); // Expects TFunctionCallRequest
        try
          ResultJson := TAIJsonUtils.MessageToJsonString(CallResult);
          FPythonBridge.SendMessageToPython(ResultJson);
          LogToConversation('Function result sent to AI: ' + CallResult.Status, clGreen);
          lblStatus.Caption := 'Status: Waiting for AI interpretation...';
        finally
          CallResult.Free;
        end;
      end
      else
      begin
        LogToConversation('AI Operation Executor not available. Cannot execute function.', clRed);
        lblStatus.Caption := 'Status: Error - Executor Missing';
        // Send error back to Python?
        CallResult := TFunctionCallResult.Create(FuncCallReq.FunctionName, 'error', '{}', FuncCallReq.RequestId, 'Pascal: AI Operation Executor not available.');
        try
          ResultJson := TAIJsonUtils.MessageToJsonString(CallResult);
          FPythonBridge.SendMessageToPython(ResultJson);
        finally
          CallResult.Free;
        end;
      end;
    end
    else if BaseMsg is TErrorMessage then // Python could send structured errors too
    begin
        ErrorMsg := TErrorMessage(BaseMsg);
        LogToConversation('AI Error: ' + ErrorMsg.Content, clRed);
        lblStatus.Caption := 'Status: AI Error Received';
    end
    // Handle other expected message types from Python if any (e.g. TUserMessage should not come from Python)
    else
    begin
      LogToConversation('AI (Unknown structured Msg Type): ' + BaseMsg.MessageType, clRed);
      lblStatus.Caption := 'Status: AI Responded (Unknown Type)';
    end;
  finally
    BaseMsg.Free; // Free the deserialized message object
  end
  else
  begin
    // Failed to parse JSON or create a BaseMessage
    LogToConversation('AI (Malformed JSON or unknown structure): ' + AMessage, clRed);
    lblStatus.Caption := 'Status: AI Responded (Malformed)';
    // Send error back to Python?
    Dim ErrorResponseToPy: TErrorMessage;
    ErrorResponseToPy := TErrorMessage.Create('Pascal: Received malformed JSON: ' + AMessage);
    try
      ResultJson := TAIJsonUtils.MessageToJsonString(ErrorResponseToPy);
      FPythonBridge.SendMessageToPython(ResultJson);
    finally
      ErrorResponseToPy.Free;
    end;
  end;
end;

end.
