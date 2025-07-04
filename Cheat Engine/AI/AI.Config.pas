unit AI.Config;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, IniFiles;

const
  DEFAULT_PYTHON_EXECUTABLE = 'python'; // Or 'python3', platform dependent. Or empty to force user setup.
  DEFAULT_AI_MODEL_ID = 'gpt-3.5-turbo';
  DEFAULT_AI_BASE_URL = 'https://api.openai.com/v1';
  DEFAULT_AI_TEMPERATURE = 0.7;
  DEFAULT_AI_MAX_TOKENS = 1500;

type
  TAIConfig = record
    PythonExecutablePath: string;
    APIBaseUrl: string;
    APIKey: string; // Note: Storing API keys in plain text config files has security implications.
    ModelId: string;
    Temperature: Double;
    MaxTokens: Integer;
    // Add other parameters as needed (e.g., TopP, PresencePenalty, FrequencyPenalty)
  end;

  TAIConfigManager = class
  private
    FConfigFilePath: string;
    FConfig: TAIConfig;
    procedure SetDefaultConfig;
  public
    constructor Create(AConfigFileName: string = 'ai_config.ini');
    procedure LoadConfig;
    procedure SaveConfig;
    function GetConfig: TAIConfig;
    procedure SetConfig(const AConfig: TAIConfig);

    class function GetDefaultConfigDir: string;
    class function GetDefaultConfigFilePath(AConfigFileName: string = 'ai_config.ini'): string;
  end;

// Global instance or function to access config manager
var
  G_AIConfigManager: TAIConfigManager = nil;

function AIConfigManager: TAIConfigManager;

implementation

uses LazFileUtils; // For GetAppConfigDir

function AIConfigManager: TAIConfigManager;
begin
  if not Assigned(G_AIConfigManager) then
    G_AIConfigManager := TAIConfigManager.Create(GetDefaultConfigFilePath());
  Result := G_AIConfigManager;
end;

{ TAIConfigManager }

constructor TAIConfigManager.Create(AConfigFileName: string);
begin
  inherited Create;
  if AConfigFileName = '' then
    FConfigFilePath := GetDefaultConfigFilePath()
  else
    FConfigFilePath := AConfigFileName;

  SetDefaultConfig; // Initialize with defaults first
  if FileExists(FConfigFilePath) then
    LoadConfig
  else
    SaveConfig; // Save defaults if file doesn't exist
end;

procedure TAIConfigManager.SetDefaultConfig;
begin
  FConfig.PythonExecutablePath := DEFAULT_PYTHON_EXECUTABLE;
  FConfig.APIBaseUrl := DEFAULT_AI_BASE_URL;
  FConfig.APIKey := ''; // Default to empty, user must fill this.
  FConfig.ModelId := DEFAULT_AI_MODEL_ID;
  FConfig.Temperature := DEFAULT_AI_TEMPERATURE;
  FConfig.MaxTokens := DEFAULT_AI_MAX_TOKENS;
end;

class function TAIConfigManager.GetDefaultConfigDir: string;
begin
  // Example: C:\Users\<User>\AppData\Local\AppName\ or ~/.config/AppName/
  // Cheat Engine might have its own preferred config directory structure.
  // For now, using Lazarus's standard GetAppConfigDir.
  // The boolean parameter true makes it create the directory if it doesn't exist.
  Result := GetAppConfigDir(True) + PathDelim + 'CheatEngineAI' + PathDelim;
  ForceDirectories(Result); // Ensure directory exists
end;

class function TAIConfigManager.GetDefaultConfigFilePath(AConfigFileName: string): string;
begin
  Result := GetDefaultConfigDir + AConfigFileName;
end;

procedure TAIConfigManager.LoadConfig;
var
  IniFile: TIniFile;
begin
  if not FileExists(FConfigFilePath) then
  begin
    SetDefaultConfig; // Should already be set by constructor if file missing
    SaveConfig; // Create it with defaults
    Exit;
  end;

  IniFile := TIniFile.Create(FConfigFilePath);
  try
    FConfig.PythonExecutablePath := IniFile.ReadString('General', 'PythonExecutable', DEFAULT_PYTHON_EXECUTABLE);
    FConfig.APIBaseUrl := IniFile.ReadString('OpenAI', 'BaseUrl', DEFAULT_AI_BASE_URL);
    FConfig.APIKey := IniFile.ReadString('OpenAI', 'APIKey', ''); // Read API Key
    FConfig.ModelId := IniFile.ReadString('OpenAI', 'ModelId', DEFAULT_AI_MODEL_ID);
    FConfig.Temperature := IniFile.ReadFloat('OpenAI', 'Temperature', DEFAULT_AI_TEMPERATURE);
    FConfig.MaxTokens := IniFile.ReadInteger('OpenAI', 'MaxTokens', DEFAULT_AI_MAX_TOKENS);
  finally
    IniFile.Free;
  end;
end;

procedure TAIConfigManager.SaveConfig;
var
  IniFile: TIniFile;
begin
  // Ensure directory exists before saving
  ForceDirectories(ExtractFilePath(FConfigFilePath));

  IniFile := TIniFile.Create(FConfigFilePath);
  try
    IniFile.WriteString('General', 'PythonExecutable', FConfig.PythonExecutablePath);
    IniFile.WriteString('OpenAI', 'BaseUrl', FConfig.APIBaseUrl);
    IniFile.WriteString('OpenAI', 'APIKey', FConfig.APIKey); // Write API Key
    IniFile.WriteString('OpenAI', 'ModelId', FConfig.ModelId);
    IniFile.WriteFloat('OpenAI', 'Temperature', FConfig.Temperature);
    IniFile.WriteInteger('OpenAI', 'MaxTokens', FConfig.MaxTokens);
    IniFile.UpdateFile; // Ensure changes are written
  finally
    IniFile.Free;
  end;
end;

function TAIConfigManager.GetConfig: TAIConfig;
begin
  Result := FConfig;
end;

procedure TAIConfigManager.SetConfig(const AConfig: TAIConfig);
begin
  FConfig := AConfig;
  // SaveConfig; // Optionally save immediately on set, or require explicit save call
end;

initialization
  // G_AIConfigManager is created on first call to AIConfigManager()

finalization
  FreeAndNil(G_AIConfigManager);

end.
