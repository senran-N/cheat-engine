unit AI.SharedTypes;

interface

uses
  SysUtils, Classes, Contnrs, typinfo,
  fpjson, jsonparser; // Standard FPC JSON units

type
  // Basic Message Types
  // Note: We will add ToJSON/FromJSON methods to these classes or use external helpers.
  TBaseMessage = class
  private
    FMessageType: string;
  public
    property MessageType: string read FMessageType write FMessageType;
    constructor Create(const AMessageType: string); virtual;
  end;

  TUserMessage = class(TBaseMessage)
  private
    FContent: string;
  public
    property Content: string read FContent write FContent;
    constructor Create(const AContent: string);
  end;

  TAIResponse = class(TBaseMessage)
  private
    FContent: string;
  public
    property Content: string read FContent write FContent;
    constructor Create(const AContent: string);
  end;

  // Function Calling Structures
  TFunctionCallParameter = class
  private
    FName: string;
    FValue: string; // Simplified for now, might need to be Variant or hold type info
  public
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    constructor Create(AName, AValue: string);
  end;

  TFunctionCallRequest = class(TBaseMessage)
  private
    FFunctionName: string;
    FParameters: TObjectList; // List of TFunctionCallParameter
    FRequestId: string; // Optional: general request correlation
    FToolCallId: string; // Specific ID from OpenAI tool_call object
  public
    property FunctionName: string read FFunctionName write FFunctionName;
    property Parameters: TObjectList read FParameters;
    property RequestId: string read FRequestId write FRequestId;
    property ToolCallId: string read FToolCallId write FToolCallId;
    constructor Create(const AFunctionName: string; ARequestId: string = ''; AToolCallId: string = '');
    destructor Destroy; override;
    procedure AddParameter(Name, Value: string);
  end;

  TFunctionCallResult = class(TBaseMessage)
  private
    FFunctionName: string;
    FStatus: string; // e.g., "success", "error"
    FResultData: string; // Could be JSON string, or a more complex type later
    FRequestId: string; // General request correlation
    FToolCallId: string; // Specific ID from OpenAI tool_call object, sent back by Pascal
    FErrorMessage: string;
  public
    property FunctionName: string read FFunctionName write FFunctionName;
    property Status: string read FStatus write FStatus;
    property ResultData: string read FResultData write FResultData;
    property RequestId: string read FRequestId write FRequestId;
    property ToolCallId: string read FToolCallId write FToolCallId;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    constructor Create(const AFunctionName, AStatus, AResultData, ARequestId: string; AToolCallId: string; AErrorMessage: string = '');
  end;

  TErrorMessage = class(TBaseMessage)
  private
    FContent: string;
    FOriginalRequestId: string; // Optional: if the error is related to a specific request
  public
    property Content: string read FContent write FContent;
    property OriginalRequestId: string read FOriginalRequestId write FOriginalRequestId;
    constructor Create(const AContent: string; AOriginalRequestId: string = '');
  end;

  // Data Structures for Function Calls (Examples)
  TProcessInfo = class
  private
    FPid: Cardinal;
    FName: string;
  public
    property Pid: Cardinal read FPid write FPid;
    property Name: string read FName write FName;
    constructor Create(APid: Cardinal; AName: string);
  end;

  TCheatTableEntryInfo = class
  private
    FIndex: Integer;
    FAddress: string;
    FType: string;
    FDescription: string;
    FValue: string;
    FIsFrozen: Boolean;
  public
    property Index: Integer read FIndex write FIndex;
    property Address: string read FAddress write FAddress;
    property Type_: string read FType write FType; // Type is a keyword
    property Description: string read FDescription write FDescription;
    property Value: string read FValue write FValue;
    property IsFrozen: Boolean read FIsFrozen write FIsFrozen;
    constructor Create(AIndex: Integer; AAddress, AType, ADescription, AValue: string; AIsFrozen: Boolean);
  end;

  // function ObjectToJsonString(Obj: TObject): string; // Will be replaced by TAIJsonUtils

  TAIJsonUtils = class
  public
    // Serialization
    class function MessageToJsonString(AMessage: TBaseMessage): string;
    class function UserMessageToJsonObject(AMsg: TUserMessage): TJSONObject;
    class function AIResponseToJsonObject(AResponse: TAIResponse): TJSONObject;
    class function FunctionCallRequestToJsonObject(ARequest: TFunctionCallRequest): TJSONObject;
    class function FunctionCallResultToJsonObject(AResult: TFunctionCallResult): TJSONObject;
    class function ErrorMessageToJsonObject(AMsg: TErrorMessage): TJSONObject;
    class function ProcessInfoToJsonObject(AInfo: TProcessInfo): TJSONObject;
    class function CheatTableEntryToJsonObject(AEntry: TCheatTableEntryInfo): TJSONObject;

    // Deserialization
    class function JsonStringToBaseMessage(const AJsonString: string; out AMessage: TBaseMessage): Boolean;
    // Specific deserializers that create and populate objects
    class function JsonObjectToUserMessage(AJsonObject: TJSONObject; out AMsg: TUserMessage): Boolean;
    class function JsonObjectToAIResponse(AJsonObject: TJSONObject; out AResponse: TAIResponse): Boolean;
    class function JsonObjectToFunctionCallRequest(AJsonObject: TJSONObject; out ARequest: TFunctionCallRequest): Boolean;
    class function JsonObjectToFunctionCallResult(AJsonObject: TJSONObject; out AResult: TFunctionCallResult): Boolean;
    class function JsonObjectToErrorMessage(AJsonObject: TJSONObject; out AErrorMsg: TErrorMessage): Boolean;

    // Utility for safe getting values
    class function GetJsonString(AObject: TJSONObject; AName: string; ADefault: string = ''): string;
    class function GetJsonInt(AObject: TJSONObject; AName: string; ADefault: Integer = 0): Integer;
    class function GetJsonBool(AObject: TJSONObject; AName: string; ADefault: Boolean = False): Boolean;
    class function GetJsonObject(AObject: TJSONObject; AName: string): TJSONObject;
    class function GetJsonArray(AObject: TJSONObject; AName: string): TJSONArray;
  end;

implementation

uses LazUTF8; // For StringToUTF8 / UTF8ToString if needed, fpjson usually handles UTF8 well.

{ TBaseMessage }
constructor TBaseMessage.Create(const AMessageType: string);
begin
  inherited Create;
  FMessageType := AMessageType;
end;

{ TUserMessage }
constructor TUserMessage.Create(const AContent: string);
begin
  inherited Create('user_message');
  FContent := AContent;
end;

{ TAIResponse }
constructor TAIResponse.Create(const AContent: string);
begin
  inherited Create('ai_response');
  FContent := AContent;
end;

{ TFunctionCallParameter }
constructor TFunctionCallParameter.Create(AName, AValue: string);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
end;

{ TFunctionCallRequest }
constructor TFunctionCallRequest.Create(const AFunctionName: string; ARequestId: string = ''; AToolCallId: string = '');
begin
  inherited Create('function_call_request');
  FFunctionName := AFunctionName;
  FParameters := TObjectList.Create(True); // True for OwnsObjects
  FRequestId := ARequestId;
  FToolCallId := AToolCallId;
end;

destructor TFunctionCallRequest.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

procedure TFunctionCallRequest.AddParameter(Name, Value: string);
begin
  FParameters.Add(TFunctionCallParameter.Create(Name, Value));
end;

{ TFunctionCallResult }
constructor TFunctionCallResult.Create(const AFunctionName, AStatus, AResultData, ARequestId: string; AToolCallId: string; AErrorMessage: string = '');
begin
  inherited Create('function_call_result');
  FFunctionName := AFunctionName;
  FStatus := AStatus;
  FResultData := AResultData; // This string itself can be a JSON string for complex data
  FRequestId := ARequestId;
  FToolCallId := AToolCallId;
  FErrorMessage := AErrorMessage;
end;

{ TErrorMessage }
constructor TErrorMessage.Create(const AContent: string; AOriginalRequestId: string = '');
begin
  inherited Create('error_message');
  FContent := AContent;
  FOriginalRequestId := AOriginalRequestId;
end;

{ TProcessInfo }
constructor TProcessInfo.Create(APid: Cardinal; AName: string);
begin
  inherited Create;
  FPid := APid;
  FName := AName;
end;

{ TCheatTableEntryInfo }
constructor TCheatTableEntryInfo.Create(AIndex: Integer; AAddress, AType, ADescription, AValue: string; AIsFrozen: Boolean);
begin
  inherited Create;
  FIndex := AIndex;
  FAddress := AAddress;
  FType := AType;
  FDescription := ADescription;
  FValue := AValue;
  FIsFrozen := AIsFrozen;
end;

{ TAIJsonUtils }

class function TAIJsonUtils.GetJsonString(AObject: TJSONObject; AName: string; ADefault: string): string;
var
  ValNode: TJSONData;
begin
  ValNode := AObject.Find(AName);
  if Assigned(ValNode) and (ValNode.JSONType = jtString) then
    Result := ValNode.AsString
  else
    Result := ADefault;
end;

class function TAIJsonUtils.GetJsonInt(AObject: TJSONObject; AName: string; ADefault: Integer): Integer;
var
  ValNode: TJSONData;
begin
  ValNode := AObject.Find(AName);
  if Assigned(ValNode) and (ValNode.JSONType in [jtNumber, jtBoolean]) then // allow bool as 0/1
    Result := ValNode.AsInteger
  else
    Result := ADefault;
end;

class function TAIJsonUtils.GetJsonBool(AObject: TJSONObject; AName: string; ADefault: Boolean): Boolean;
var
  ValNode: TJSONData;
begin
  ValNode := AObject.Find(AName);
  if Assigned(ValNode) and (ValNode.JSONType in [jtBoolean, jtNumber, jtString]) then
  begin
    if ValNode.JSONType = jtBoolean then Result := ValNode.AsBoolean
    else if ValNode.JSONType = jtNumber then Result := ValNode.AsInteger <> 0
    else Result := SameText(ValNode.AsString, 'true') or (ValNode.AsString = '1');
  end
  else
    Result := ADefault;
end;

class function TAIJsonUtils.GetJsonObject(AObject: TJSONObject; AName: string): TJSONObject;
var
  ValNode: TJSONData;
begin
  Result := nil;
  ValNode := AObject.Find(AName);
  if Assigned(ValNode) and (ValNode is TJSONObject) then
    Result := TJSONObject(ValNode);
end;

class function TAIJsonUtils.GetJsonArray(AObject: TJSONObject; AName: string): TJSONArray;
var
  ValNode: TJSONData;
begin
  Result := nil;
  ValNode := AObject.Find(AName);
  if Assigned(ValNode) and (ValNode is TJSONArray) then
    Result := TJSONArray(ValNode);
end;

class function TAIJsonUtils.MessageToJsonString(AMessage: TBaseMessage): string;
var
  JsonObj: TJSONObject;
begin
  Result := '{}'; // Default empty JSON
  if not Assigned(AMessage) then Exit;

  JsonObj := nil;
  try
    if AMessAge is TUserMessage then
      JsonObj := UserMessageToJsonObject(TUserMessage(AMessage))
    else if AMessage is TAIResponse then
      JsonObj := AIResponseToJsonObject(TAIResponse(AMessage))
    else if AMessage is TFunctionCallRequest then
      JsonObj := FunctionCallRequestToJsonObject(TFunctionCallRequest(AMessage))
    else if AMessage is TFunctionCallResult then
      JsonObj := FunctionCallResultToJsonObject(TFunctionCallResult(AMessage))
    else if AMessage is TErrorMessage then
      JsonObj := ErrorMessageToJsonObject(TErrorMessage(AMessage))
    else
    begin
      JsonObj := TJSONObject.Create;
      JsonObj.Add('message_type', AMessage.MessageType);
      JsonObj.Add('error', 'Unknown message type for detailed JSON serialization');
    end;

    if Assigned(JsonObj) then
      Result := JsonObj.FormatJSON() // Use FormatJSON for pretty print, or AsJSON for compact
    else
      Result := '{}'; // Should not happen if logic is correct
  finally
    JsonObj.Free; // The created JSON object should always be freed
  end;
end;

class function TAIJsonUtils.UserMessageToJsonObject(AMsg: TUserMessage): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('message_type', AMsg.MessageType); // "user_message"
  Result.Add('content', AMsg.Content);
end;

class function TAIJsonUtils.AIResponseToJsonObject(AResponse: TAIResponse): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('message_type', AResponse.MessageType); // "ai_response"
  Result.Add('content', AResponse.Content);
end;

class function TAIJsonUtils.FunctionCallRequestToJsonObject(ARequest: TFunctionCallRequest): TJSONObject;
var
  ParamsObject: TJSONObject;
  I: Integer;
  Param: TFunctionCallParameter;
  ParamValueJson: TJSONData;
begin
  Result := TJSONObject.Create;
  Result.Add('message_type', ARequest.MessageType); // "function_call_request"
  Result.Add('function_name', ARequest.FunctionName);
  if ARequest.RequestId <> '' then
    Result.Add('request_id', ARequest.RequestId);
  if ARequest.ToolCallId <> '' then
    Result.Add('tool_call_id', ARequest.ToolCallId);

  ParamsObject := TJSONObject.Create;
  for I := 0 to ARequest.Parameters.Count - 1 do
  begin
    Param := ARequest.Parameters[I] as TFunctionCallParameter;
    // Attempt to parse Param.Value as JSON if it looks like it.
    // Otherwise, add as a string. This makes parameters more flexible.
    try
      if ((Param.Value <> '') and ((Param.Value[1] = '{') or (Param.Value[1] = '['))) or
         (TryStrToInt(Param.Value, var _)) or (SameText(Param.Value, 'true')) or (SameText(Param.Value, 'false')) then
        ParamValueJson := GetJSON(Param.Value) // Try to parse string as JSON primitive/object/array
      else
        ParamValueJson := TJSONString.Create(Param.Value); // Treat as plain string
    except
      ParamValueJson := TJSONString.Create(Param.Value); // Fallback to string if GetJSON fails
    end;
    ParamsObject.Add(Param.Name, ParamValueJson); // Add directly, TJSONObject handles freeing if it owns it
  end;
  Result.Add('parameters', ParamsObject);
end;

class function TAIJsonUtils.FunctionCallResultToJsonObject(AResult: TFunctionCallResult): TJSONObject;
var
  ResultDataJson: TJSONData;
begin
  Result := TJSONObject.Create;
  Result.Add('message_type', AResult.MessageType); // "function_call_result"
  Result.Add('function_name', AResult.FunctionName);
  Result.Add('status', AResult.Status);

  try
    // AResult.ResultData is a string, which might be simple string, or JSON itself.
    // If it's JSON, parse it and add as object/array, otherwise add as string.
    if ((AResult.ResultData <> '') and ((AResult.ResultData[1] = '{') or (AResult.ResultData[1] = '['))) then
       ResultDataJson := GetJSON(AResult.ResultData) // Parse it as JSON
    else
       ResultDataJson := TJSONString.Create(AResult.ResultData); // Treat as plain string
  except
    ResultDataJson := TJSONString.Create(AResult.ResultData); // Fallback to plain string on parse error
  end;
  Result.Add('result_data', ResultDataJson); // Add TJSONData, it will be freed by parent TJSONObject

  if AResult.RequestId <> '' then
    Result.Add('request_id', AResult.RequestId);
  if AResult.ToolCallId <> '' then
    Result.Add('tool_call_id', AResult.ToolCallId);
  if AResult.ErrorMessage <> '' then
    Result.Add('error_message', AResult.ErrorMessage);
end;

class function TAIJsonUtils.ErrorMessageToJsonObject(AMsg: TErrorMessage): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('message_type', AMsg.MessageType); // "error_message"
  Result.Add('content', AMsg.Content);
  if AMsg.OriginalRequestId <> '' then
    Result.Add('original_request_id', AMsg.OriginalRequestId);
end;

class function TAIJsonUtils.ProcessInfoToJsonObject(AInfo: TProcessInfo): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('pid', Cardinal(AInfo.Pid));
  Result.Add('name', AInfo.Name);
end;

class function TAIJsonUtils.CheatTableEntryToJsonObject(AEntry: TCheatTableEntryInfo): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('index', AEntry.Index);
  Result.Add('address', AEntry.Address);
  Result.Add('type', AEntry.Type_);
  Result.Add('description', AEntry.Description);
  Result.Add('value', AEntry.Value);
  Result.Add('is_frozen', AEntry.IsFrozen);
end;

// DESERIALIZATION

class function TAIJsonUtils.JsonStringToBaseMessage(const AJsonString: string; out AMessage: TBaseMessage): Boolean;
var
  LJsonParser: TJSONParser;
  JsonObj: TJSONObject;
  MsgType: string;
begin
  Result := False;
  AMessage := nil;
  LJsonParser := nil;
  JsonObj := nil;

  try
    LJsonParser := TJSONParser.Create(AJsonString);
    JsonObj := TJSONObject(LJsonParser.Parse); // Parse returns TJSONData, cast if sure it's an object

    if not Assigned(JsonObj) or not (JsonObj is TJSONObject) then
    begin
      // writeln('Failed to parse JSON string or not a JSON object.');
      Exit;
    end;

    MsgType := GetJsonString(JsonObj, 'message_type');

    if MsgType = 'user_message' then
    begin
      Dim LUserMessage: TUserMessage;
      Result := JsonObjectToUserMessage(JsonObj, LUserMessage);
      if Result then AMessage := LUserMessage else LUserMessage.Free; // Free if not assigned
    end
    else if MsgType = 'ai_response' then
    begin
      Dim LAIResponse: TAIResponse;
      Result := JsonObjectToAIResponse(JsonObj, LAIResponse);
      if Result then AMessage := LAIResponse else LAIResponse.Free;
    end
    else if MsgType = 'function_call_request' then
    begin
      Dim LFuncRequest: TFunctionCallRequest;
      Result := JsonObjectToFunctionCallRequest(JsonObj, LFuncRequest);
      if Result then AMessage := LFuncRequest else LFuncRequest.Free;
    end
    else if MsgType = 'function_call_result' then
    begin
      Dim LFuncResult: TFunctionCallResult;
      Result := JsonObjectToFunctionCallResult(JsonObj, LFuncResult);
      if Result then AMessage := LFuncResult else LFuncResult.Free;
    end
    else if MsgType = 'error_message' then
    begin
      Dim LErrorMsg: TErrorMessage;
      Result := JsonObjectToErrorMessage(JsonObj, LErrorMsg);
      if Result then AMessage := LErrorMsg else LErrorMsg.Free;
    end
    else
    begin
      // writeln('Unknown message_type: ' + MsgType);
      Result := False;
    end;

  except
    on E: Exception do
    begin
      // writeln('Exception in JsonStringToBaseMessage: ' + E.Message);
      Result := False;
      if Assigned(AMessage) then FreeAndNil(AMessage); // Ensure partially created message is freed
    end;
  end;
  // JsonObj is owned by LJsonParser if parsing was successful.
  // LJsonParser and its owned data (JsonObj) are freed here.
  LJsonParser.Free;
end;

class function TAIJsonUtils.JsonObjectToUserMessage(AJsonObject: TJSONObject; out AMsg: TUserMessage): Boolean;
begin
  Result := False;
  AMsg := nil;
  if not Assigned(AJsonObject) then Exit;
  try
    AMsg := TUserMessage.Create(GetJsonString(AJsonObject, 'content'));
    Result := True;
  except
    on E: Exception do begin FreeAndNil(AMsg); Result := False; end;
  end;
end;

class function TAIJsonUtils.JsonObjectToAIResponse(AJsonObject: TJSONObject; out AResponse: TAIResponse): Boolean;
begin
  Result := False;
  AResponse := nil;
  if not Assigned(AJsonObject) then Exit;
  try
    AResponse := TAIResponse.Create(GetJsonString(AJsonObject, 'content'));
    Result := True;
  except
    on E: Exception do begin FreeAndNil(AResponse); Result := False; end;
  end;
end;

class function TAIJsonUtils.JsonObjectToFunctionCallRequest(AJsonObject: TJSONObject; out ARequest: TFunctionCallRequest): Boolean;
var
  FuncName, ReqId, ToolCallId: string;
  ParamsObj: TJSONObject;
  I: Integer;
  ParamName: String;
  ParamValueNode: TJSONData;
begin
  Result := False;
  ARequest := nil;
  if not Assigned(AJsonObject) then Exit;

  try
    FuncName := GetJsonString(AJsonObject, 'function_name');
    ReqId := GetJsonString(AJsonObject, 'request_id');
    ToolCallId := GetJsonString(AJsonObject, 'tool_call_id');

    if FuncName = '' then Exit; // Function name is mandatory

    ARequest := TFunctionCallRequest.Create(FuncName, ReqId, ToolCallId);

    ParamsObj := GetJsonObject(AJsonObject, 'parameters');
    if Assigned(ParamsObj) then
    begin
      for I := 0 to ParamsObj.Count - 1 do
      begin
        ParamName := ParamsObj.Names[I];
        ParamValueNode := ParamsObj.Items[I];
        // Store parameter value as its JSON string representation for simplicity,
        // or convert to simple string if it's a JSONString.
        if ParamValueNode is TJSONString then
          ARequest.AddParameter(ParamName, TJSONString(ParamValueNode).AsString)
        else
          ARequest.AddParameter(ParamName, ParamValueNode.AsJSON);
      end;
    end;
    Result := True;
  except
    on E: Exception do begin FreeAndNil(ARequest); Result := False; end;
  end;
end;

class function TAIJsonUtils.JsonObjectToFunctionCallResult(AJsonObject: TJSONObject; out AResult: TFunctionCallResult): Boolean;
var
  FuncName, Status, ResultDataStr, ReqId, ErrorMsg, ToolCallId: string;
  ResultDataNode: TJSONData;
begin
  Result := False;
  AResult := nil;
  if not Assigned(AJsonObject) then Exit;

  try
    FuncName := GetJsonString(AJsonObject, 'function_name');
    Status := GetJsonString(AJsonObject, 'status', 'error');
    ReqId := GetJsonString(AJsonObject, 'request_id');
    ToolCallId := GetJsonString(AJsonObject, 'tool_call_id');
    ErrorMsg := GetJsonString(AJsonObject, 'error_message');

    if FuncName = '' then Exit; // Function name is mandatory

    ResultDataNode := AJsonObject.Find('result_data');
    if Assigned(ResultDataNode) then
      ResultDataStr := ResultDataNode.AsJSON // Store the raw JSON string of result_data
    else
      ResultDataStr := '{}'; // Default to empty object string if not found

    AResult := TFunctionCallResult.Create(FuncName, Status, ResultDataStr, ReqId, ToolCallId, ErrorMsg);
    Result := True;
  except
    on E: Exception do begin FreeAndNil(AResult); Result := False; end;
  end;
end;

class function TAIJsonUtils.JsonObjectToErrorMessage(AJsonObject: TJSONObject; out AErrorMsg: TErrorMessage): Boolean;
begin
  Result := False;
  AErrorMsg := nil;
  if not Assigned(AJsonObject) then Exit;
  try
    AErrorMsg := TErrorMessage.Create(
      GetJsonString(AJsonObject, 'content'),
      GetJsonString(AJsonObject, 'original_request_id')
    );
    Result := True;
  except
    on E: Exception do begin FreeAndNil(AErrorMsg); Result := False; end;
  end;
end;

initialization
  // Initialization code, if any

finalization
  // Finalization code, if any

end.
