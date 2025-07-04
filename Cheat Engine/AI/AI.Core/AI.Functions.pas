unit AI.Functions;

interface

uses
  SysUtils, Classes, Contnrs, AI.SharedTypes; // AI.SharedTypes for parameter types if needed

type
  // Represents a parameter definition for an AI callable function
  TAIFunctionParameterDef = class
  private
    FName: string;
    FParamType: string; // e.g., "string", "integer", "boolean", "number" (for JSON schema)
    FDescription: string;
    FIsRequired: Boolean;
  public
    property Name: string read FName write FName;
    property ParamType: string read FParamType write FParamType;
    property Description: string read FDescription write FDescription;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
    constructor Create(AName, AParamType, ADescription: string; AIsRequired: Boolean = True);
  end;

  // Represents an AI callable function definition
  TAIFunctionDefinition = class
  private
    FName: string;
    FDescription: string;
    FParameters: TObjectList; // List of TAIFunctionParameterDef
    // FExecutor: TMethod; // Could store a direct method pointer or a lookup key
  public
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Parameters: TObjectList read FParameters;
    constructor Create(AName, ADescription: string);
    destructor Destroy; override;
    procedure AddParameter(AParamName, AParamType, AParamDescription: string; AIsRequired: Boolean = True);
    function ToJsonSchemaString: string; // Generates JSON schema for this function
  end;

  // Manages all registered AI functions
  TAIFunctionRegistry = class
  private
    FFunctions: TObjectDictionary; // Maps function name (string) to TAIFunctionDefinition
    class var FInstance: TAIFunctionRegistry;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetInstance: TAIFunctionRegistry;
    procedure RegisterFunction(ADefinition: TAIFunctionDefinition);
    function GetFunction(AName: string): TAIFunctionDefinition;
    function GetAllFunctionsSchemaJson: string; // Generates JSON array of all function schemas
    procedure UnregisterFunction(AName: string);
    procedure LoadStandardFunctions; // Method to load predefined functions
  end;

// Global accessor
function AIFunctions: TAIFunctionRegistry;

implementation

{ TAIFunctionParameterDef }
constructor TAIFunctionParameterDef.Create(AName, AParamType, ADescription: string; AIsRequired: Boolean);
begin
  inherited Create;
  FName := AName;
  FParamType := AParamType;
  FDescription := ADescription;
  FIsRequired := AIsRequired;
end;

{ TAIFunctionDefinition }
constructor TAIFunctionDefinition.Create(AName, ADescription: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FParameters := TObjectList.Create(True); // Owns TAIFunctionParameterDef objects
end;

destructor TAIFunctionDefinition.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

procedure TAIFunctionDefinition.AddParameter(AParamName, AParamType, AParamDescription: string; AIsRequired: Boolean);
begin
  FParameters.Add(TAIFunctionParameterDef.Create(AParamName, AParamType, AParamDescription, AIsRequired));
end;

function TAIFunctionDefinition.ToJsonSchemaString: string;
var
  I: Integer;
  ParamDef: TAIFunctionParameterDef;
  RootJson, ParamsJson, PropsJson: TJSONObject;
  RequiredArrayJson: TJSONArray;
begin
  RootJson := TJSONObject.Create;
  try
    RootJson.Add('name', Self.FName);
    RootJson.Add('description', Self.FDescription);

    ParamsJson := TJSONObject.Create;
    ParamsJson.Add('type', 'object');

    PropsJson := TJSONObject.Create;
    RequiredArrayJson := TJSONArray.Create;

    for I := 0 to Self.FParameters.Count - 1 do
    begin
      ParamDef := Self.FParameters[I] as TAIFunctionParameterDef;
      Dim PropDefJson: TJSONObject = TJSONObject.Create;
      PropDefJson.Add('type', ParamDef.ParamType);
      PropDefJson.Add('description', ParamDef.Description);
      PropsJson.Add(ParamDef.Name, PropDefJson); // Add PropDefJson, PropsJson takes ownership

      if ParamDef.IsRequired then
      begin
        RequiredArrayJson.Add(ParamDef.Name);
      end;
    end;

    ParamsJson.Add('properties', PropsJson); // Add PropsJson, ParamsJson takes ownership
    if RequiredArrayJson.Count > 0 then
    begin
      ParamsJson.Add('required', RequiredArrayJson); // Add RequiredArrayJson, ParamsJson takes ownership
    end
    else
    begin
      RequiredArrayJson.Free; // Not used, so free it
    end;

    RootJson.Add('parameters', ParamsJson); // Add ParamsJson, RootJson takes ownership
    Result := RootJson.FormatJSON(); // Or AsJSON for compact
  finally
    RootJson.Free; // This will free all owned sub-objects
  end;
end;

{ TAIFunctionRegistry }
constructor TAIFunctionRegistry.Create;
begin
  inherited Create;
  FFunctions := TObjectDictionary.Create([doOwnsValues]); // Owns TAIFunctionDefinition objects
end;

destructor TAIFunctionRegistry.Destroy;
begin
  FFunctions.Free;
  inherited Destroy;
end;

class function TAIFunctionRegistry.GetInstance: TAIFunctionRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TAIFunctionRegistry.Create;
  Result := FInstance;
end;

procedure TAIFunctionRegistry.RegisterFunction(ADefinition: TAIFunctionDefinition);
begin
  if Assigned(ADefinition) then
  begin
    if FFunctions.ContainsKey(ADefinition.Name) then
      raise Exception.CreateFmt('Function "%s" is already registered.', [ADefinition.Name]);
    FFunctions.Add(ADefinition.Name, ADefinition);
  end;
end;

function TAIFunctionRegistry.GetFunction(AName: string): TAIFunctionDefinition;
begin
  if FFunctions.TryGetValue(AName, Pointer(Result)) then // Type cast to Pointer then to TAIFunctionDefinition
  begin
    // Result is already set by TryGetValue if found
  end
  else
    Result := nil;
end;

function TAIFunctionRegistry.GetAllFunctionsSchemaJson: string;
var
  Key: string;
  FuncDef: TAIFunctionDefinition;
  AllSchemas: TStringList;
begin
  AllSchemas := TStringList.Create;
  try
    for Key in FFunctions.Keys do
    begin
      if FFunctions.TryGetValue(Key, Pointer(FuncDef)) then // Correctly retrieve the object
      begin
        AllSchemas.Add(FuncDef.ToJsonSchemaString);
      end;
    end;
    Result := '[' + AllSchemas.CommaText + ']';
  finally
    AllSchemas.Free;
  end;
end;

procedure TAIFunctionRegistry.UnregisterFunction(AName: string);
begin
  FFunctions.Remove(AName); // This will also free the object if doOwnsValues is true
end;

procedure TAIFunctionRegistry.LoadStandardFunctions;
var
  FuncDef: TAIFunctionDefinition;
begin
  // Stub for listProcesses
  FuncDef := TAIFunctionDefinition.Create('listProcesses', 'Get current running processes.');
  // No parameters for listProcesses
  RegisterFunction(FuncDef);

  // Stub for openProcess
  FuncDef := TAIFunctionDefinition.Create('openProcess', 'Open a process by its PID.');
  FuncDef.AddParameter('pid', 'integer', 'The Process ID to open.', True);
  RegisterFunction(FuncDef);

  // Add more function definitions here as per the guide
  // e.g., getAttachedProcess
  FuncDef := TAIFunctionDefinition.Create('getAttachedProcess', 'Get the currently attached process info.');
  RegisterFunction(FuncDef);

  // Scan functions
  FuncDef := TAIFunctionDefinition.Create('startFirstScan', 'Starts a new initial memory scan.');
  FuncDef.AddParameter('value', 'string', 'The value to scan for (e.g., "100", "12.34", "text").', True);
  FuncDef.AddParameter('type', 'string', 'The scan data type (e.g., "4 Bytes", "Float", "String").', True);
  FuncDef.AddParameter('scan_option', 'string', 'Scan option (e.g., "Exact Value", "Increased Value", "Decreased Value", "Between").', True);
  // Optional parameters for scan_option="Between"
  FuncDef.AddParameter('value2', 'string', 'The second value for "Between" scans.', False);
  // Optional parameters for scan range and protection, etc. (Can be added later if needed by AI)
  // FuncDef.AddParameter('start_address', 'string', 'Hex start address for scan range (optional).', False);
  // FuncDef.AddParameter('end_address', 'string', 'Hex end address for scan range (optional).', False);
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('getScanStatus', 'Queries the status of an ongoing or completed scan task.');
  FuncDef.AddParameter('task_id', 'string', 'The ID of the scan task to query.', True);
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('getScanResults', 'Retrieves results from a completed scan task.');
  FuncDef.AddParameter('task_id', 'string', 'The ID of the scan task.', True);
  FuncDef.AddParameter('offset', 'integer', 'The starting offset for results (for pagination).', False);
  FuncDef.AddParameter('count', 'integer', 'The maximum number of results to return (for pagination).', False);
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('clearScan', 'Clears a scan task and its results.');
  FuncDef.AddParameter('task_id', 'string', 'The ID of the scan task to clear.', True);
  RegisterFunction(FuncDef);

  // Placeholder for nextScan and AOBScan, can be detailed later
  FuncDef := TAIFunctionDefinition.Create('startNextScan', 'Performs a subsequent scan based on previous results.');
  FuncDef.AddParameter('task_id', 'string', 'The ID of the existing scan task.', True);
  FuncDef.AddParameter('value', 'string', 'The new value for the next scan.', True);
  // FuncDef.AddParameter('scan_option', 'string', 'Scan option for next scan.', True); // Often implied or simpler for next scan
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('startAOBScan', 'Starts an Array of Bytes (AOB) scan.');
  FuncDef.AddParameter('aob', 'string', 'The AOB pattern (e.g., "41 42 ?? 45").', True);
  RegisterFunction(FuncDef);

  // Address List / Cheat Table functions
  FuncDef := TAIFunctionDefinition.Create('addAddressToCheatTable', 'Adds an address to the Cheat Table.');
  FuncDef.AddParameter('address', 'string', 'The memory address (e.g., "0x123ABC").', True);
  FuncDef.AddParameter('type', 'string', 'The data type (e.g., "4 Bytes", "Float", "String").', True);
  FuncDef.AddParameter('description', 'string', 'A description for this address entry.', False); // Optional description
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('getCheatTableEntries', 'Retrieves all entries from the current Cheat Table.');
  // No parameters
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('setCheatTableEntryValue', 'Sets the value of a specific entry in the Cheat Table.');
  FuncDef.AddParameter('index', 'integer', 'The index of the entry in the cheat table.', True);
  FuncDef.AddParameter('value', 'string', 'The new value to set for the entry.', True);
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('setCheatTableEntryDescription', 'Sets the description of a specific entry in the Cheat Table.');
  FuncDef.AddParameter('index', 'integer', 'The index of the entry in the cheat table.', True);
  FuncDef.AddParameter('description', 'string', 'The new description for the entry.', True);
  RegisterFunction(FuncDef);

  FuncDef := TAIFunctionDefinition.Create('toggleFreezeAddress', 'Toggles the freeze state of a specific entry in the Cheat Table.');
  FuncDef.AddParameter('index', 'integer', 'The index of the entry in the cheat table.', True);
  FuncDef.AddParameter('freeze', 'boolean', 'True to freeze the address, False to unfreeze.', True);
  RegisterFunction(FuncDef);

end;

function AIFunctions: TAIFunctionRegistry;
begin
  Result := TAIFunctionRegistry.GetInstance;
end;

initialization
  // FInstance is created on first call to GetInstance

finalization
  if Assigned(TAIFunctionRegistry.FInstance) then
    FreeAndNil(TAIFunctionRegistry.FInstance);

end.
