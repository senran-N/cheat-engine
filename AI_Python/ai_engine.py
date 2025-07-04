import sys
import json
import datetime
import os
from openai import OpenAI, APIError, APIConnectionError, APITimeoutError

# --- Configuration ---
# Load from environment variables or a config file
# For simplicity, using environment variables here.
# Users should set these in their environment.
OPENAI_API_KEY = os.environ.get("OPENAI_API_KEY")
OPENAI_BASE_URL = os.environ.get("OPENAI_BASE_URL", "https://api.openai.com/v1") # Default to OpenAI
OPENAI_MODEL_ID = os.environ.get("OPENAI_MODEL_ID", "gpt-3.5-turbo") # Default model

# Common adjustable AI parameters (can be overridden by config later)
DEFAULT_TEMPERATURE = 0.7
DEFAULT_MAX_TOKENS = 1500

LOG_FILE_PATH = os.path.join(os.path.dirname(__file__), "ai_engine.log")

# --- Globals ---
client = None
conversation_history = []
available_tools = [] # Will be populated by Pascal side via a system_init message

# Placeholder for function definitions that Pascal will send
# Example structure:
# available_tools = [
#     {
#         "type": "function",
#         "function": {
#             "name": "listProcesses",
#             "description": "Get current running processes.",
#             "parameters": {
#                 "type": "object",
#                 "properties": {}, # No parameters for listProcesses
#             }
#         }
#     },
#     # ... other functions
# ]

def log_message(message):
    """Appends a timestamped message to the log file."""
    try:
        with open(LOG_FILE_PATH, "a", encoding="utf-8") as f:
            timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            f.write(f"[{timestamp}] {message}\n")
    except Exception as e:
        # Fallback if logging to file fails (e.g. permissions)
        print(f"LOGGING ERROR: {e} - Original message: {message}", file=sys.stderr, flush=True)

def send_response(response_data):
    """Sends a JSON response to stdout."""
    try:
        json_response = json.dumps(response_data)
        print(json_response, flush=True)
        log_message(f"Sent: {json_response}")
    except Exception as e:
        log_message(f"Error sending response: {e} - Data: {response_data}")
        error_resp = json.dumps({"message_type": "error", "content": f"Python error during send: {str(e)}"})
        print(error_resp, flush=True)

def initialize_openai_client():
    global client
    if not OPENAI_API_KEY:
        log_message("ERROR: OPENAI_API_KEY environment variable not set.")
        send_response({
            "message_type": "error",
            "content": "Python AI: OPENAI_API_KEY is not configured."
        })
        return False

    try:
        client = OpenAI(api_key=OPENAI_API_KEY, base_url=OPENAI_BASE_URL)
        log_message(f"OpenAI client initialized. Model: {OPENAI_MODEL_ID}, Base URL: {OPENAI_BASE_URL}")
        # A small test call could be made here to verify credentials if desired
        # client.models.list()
        return True
    except Exception as e:
        log_message(f"Failed to initialize OpenAI client: {e}")
        send_response({
            "message_type": "error",
            "content": f"Python AI: Failed to initialize OpenAI client: {str(e)}"
        })
        return False

def add_to_conversation(role, content, tool_calls=None, tool_call_id=None, name=None):
    """Adds a message to the conversation history."""
    message = {"role": role, "content": content}
    if tool_calls: # For assistant message with tool calls
        message["tool_calls"] = tool_calls
    if tool_call_id: # For tool role message
        message["tool_call_id"] = tool_call_id
        # 'name' is implicitly the function name for tool role, but openai lib expects content from tool
    if name and role == "tool": # name of the function for tool role
        message["name"] = name

    conversation_history.append(message)
    # Optional: Trim history if it gets too long
    # MAX_HISTORY_LEN = 20
    # if len(conversation_history) > MAX_HISTORY_LEN:
    #     conversation_history = conversation_history[-MAX_HISTORY_LEN:]
    log_message(f"Added to history: {message}")


def handle_user_message(data):
    global client
    if not client and not initialize_openai_client():
        # If client initialization failed previously and still fails,
        # send an error message back or a fallback.
        send_response({
            "message_type": "ai_response",
            "content": "I'm sorry, my connection to the AI brain is not working. Please check the configuration.",
            "request_id": data.get("request_id", "")
        })
        return

    user_content = data.get("content", "")
    request_id = data.get("request_id", f"req_{datetime.datetime.now().timestamp()}")

    add_to_conversation("user", user_content)

    try:
        log_message(f"Calling OpenAI. History: {json.dumps(conversation_history[-5:])}") # Log last 5 for brevity

        api_params = {
            "model": OPENAI_MODEL_ID,
            "messages": conversation_history,
            "temperature": DEFAULT_TEMPERATURE,
            "max_tokens": DEFAULT_MAX_TOKENS,
        }
        if available_tools: # Only send tools if we have them
            api_params["tools"] = available_tools
            api_params["tool_choice"] = "auto"

        chat_completion = client.chat.completions.create(**api_params)

        log_message(f"OpenAI response: {chat_completion.model_dump_json(indent=2)}")

        assistant_message = chat_completion.choices[0].message

        # Check for tool calls
        if assistant_message.tool_calls:
            add_to_conversation("assistant", assistant_message.content or "", tool_calls=[tc.model_dump() for tc in assistant_message.tool_calls])
            for tool_call in assistant_message.tool_calls:
                function_name = tool_call.function.name
                function_args_str = tool_call.function.arguments
                try:
                    function_args = json.loads(function_args_str)
                except json.JSONDecodeError:
                    log_message(f"Error: AI returned invalid JSON for function arguments: {function_args_str}")
                    # Send error back to AI or user? For now, send error to Pascal as if it's an AI response.
                    send_response({
                        "message_type": "ai_response",
                        "content": f"AI tried to call function '{function_name}' but provided malformed arguments.",
                        "request_id": request_id
                    })
                    continue # next tool_call or finish

                function_call_payload = {
                    "message_type": "function_call_request",
                    "function_name": function_name,
                    "parameters": function_args, # This should be a dict
                    "request_id": request_id, # Propagate original request_id
                    "tool_call_id": tool_call.id # Important for matching results
                }
                send_response(function_call_payload)
        else:
            # Regular text response
            ai_content = assistant_message.content or "I don't have a specific response for that."
            add_to_conversation("assistant", ai_content)
            response = {
                "message_type": "ai_response",
                "content": ai_content,
                "request_id": request_id
            }
            send_response(response)

    except APIConnectionError as e:
        log_message(f"OpenAI APIConnectionError: {e}")
        send_response({"message_type": "ai_response", "content": "I couldn't connect to the AI service. Please check the network and Base URL.", "request_id": request_id})
    except APITimeoutError as e:
        log_message(f"OpenAI APITimeoutError: {e}")
        send_response({"message_type": "ai_response", "content": "The request to the AI service timed out. Please try again.", "request_id": request_id})
    except APIError as e: # General OpenAI API error (e.g. invalid key, rate limits)
        log_message(f"OpenAI APIError: Status {e.status_code} - {e.message}")
        err_content = f"An error occurred with the AI service: {e.message}"
        if e.status_code == 401: # Unauthorized
            err_content = "AI Service Authentication Failed. Please check your API Key."
        elif e.status_code == 429: # Rate limit
            err_content = "AI Service rate limit exceeded. Please try again later."
        send_response({"message_type": "ai_response", "content": err_content, "request_id": request_id})
    except Exception as e:
        log_message(f"Error during OpenAI call: {e}")
        send_response({
            "message_type": "ai_response",
            "content": f"An unexpected error occurred while thinking: {str(e)}",
            "request_id": request_id
        })


def handle_function_call_result(data):
    global client
    if not client: # Should have been initialized by a user message first
        log_message("Error: Received function_call_result before OpenAI client was initialized.")
        # Send a generic error or ignore?
        return

    function_name = data.get("function_name", "unknown_function")
    status = data.get("status", "unknown_status")
    result_data_str = data.get("result_data", "{}") # Pascal sends this as a string, could be JSON string
    error_msg = data.get("error_message", "")
    request_id = data.get("request_id", "")
    tool_call_id = data.get("tool_call_id", "") # Expecting this from Pascal now

    if not tool_call_id:
        log_message("Error: Received function_call_result without a tool_call_id. Cannot correlate with AI request.")
        # We could try to find the last tool_call for this function_name, but it's risky.
        # For now, send an AI message indicating confusion.
        send_response({
            "message_type": "ai_response",
            "content": f"AI received a result for function '{function_name}' but is unsure which request it belongs to.",
            "request_id": request_id
        })
        return

    # Content for the 'tool' role message should be the result of the tool call.
    # OpenAI expects this content to be a string.
    # If status is error, the error_msg is more relevant. Otherwise, result_data_str.
    tool_content = ""
    if status == "success":
        tool_content = result_data_str # This is the JSON string from Pascal
    else:
        tool_content = f"Error executing function: {error_msg}"
        if result_data_str and result_data_str != '{}': # include data if any
             tool_content += f" (Details: {result_data_str})"

    add_to_conversation(role="tool", content=tool_content, tool_call_id=tool_call_id, name=function_name)

    try:
        log_message(f"Calling OpenAI with tool result. History: {json.dumps(conversation_history[-5:])}")

        api_params = {
            "model": OPENAI_MODEL_ID,
            "messages": conversation_history,
            "temperature": DEFAULT_TEMPERATURE,
            "max_tokens": DEFAULT_MAX_TOKENS,
        }
        if available_tools:
            api_params["tools"] = available_tools
            api_params["tool_choice"] = "auto"

        chat_completion = client.chat.completions.create(**api_params)
        log_message(f"OpenAI response after tool call: {chat_completion.model_dump_json(indent=2)}")

        assistant_message = chat_completion.choices[0].message

        # AI should not call another function immediately after a tool result in most simple flows,
        # but we handle it just in case for robustness.
        if assistant_message.tool_calls:
            add_to_conversation("assistant", assistant_message.content or "", tool_calls=[tc.model_dump() for tc in assistant_message.tool_calls])
            # Recurse or send new function calls. For simplicity, just send the first one if any.
            # This part might need more sophisticated state management if multiple tool calls are chained.
            log_message("Warning: AI requested another tool call immediately after a tool result. Handling first one.")
            # For this basic version, we'll just process the first tool call if any.
            # A more robust version might loop or have a state machine.
            first_tool_call = assistant_message.tool_calls[0]
            f_name = first_tool_call.function.name
            f_args = json.loads(first_tool_call.function.arguments)
            send_response({
                "message_type": "function_call_request",
                "function_name": f_name,
                "parameters": f_args,
                "request_id": request_id, # Re-use request_id for context
                "tool_call_id": first_tool_call.id
            })
        else:
            ai_content = assistant_message.content or "Okay, done."
            add_to_conversation("assistant", ai_content)
            response = {
                "message_type": "ai_response",
                "content": ai_content,
                "request_id": request_id
            }
            send_response(response)

    except APIError as e:
        log_message(f"OpenAI APIError after tool result: {e}")
        send_response({"message_type": "ai_response", "content": f"An AI service error occurred: {e.message}", "request_id": request_id})
    except Exception as e:
        log_message(f"Error during OpenAI call after tool result: {e}")
        send_response({"message_type": "ai_response", "content": f"An unexpected error occurred: {str(e)}", "request_id": request_id})


def handle_system_init(data):
    """Handles system initialization message from Pascal, typically containing tool definitions."""
    global available_tools
    tools_data = data.get("tools", [])
    if isinstance(tools_data, list):
        # Validate structure if necessary, for now assume it's correct JSON schema for tools
        parsed_tools = []
        for tool_dict in tools_data:
            if isinstance(tool_dict, dict) and tool_dict.get("type") == "function" and isinstance(tool_dict.get("function"), dict):
                 parsed_tools.append(tool_dict)
            else:
                log_message(f"Warning: Invalid tool structure received in system_init: {tool_dict}")

        available_tools = parsed_tools
        log_message(f"Received and updated available tools: {json.dumps(available_tools)}")
        # Optionally send an acknowledgement or a status update if needed
        # send_response({"message_type": "system_ack", "status": "tools_updated"})
    else:
        log_message(f"Warning: 'tools' field in system_init is not a list: {tools_data}")


def main():
    log_message("AI Engine (OpenAI Integrated) started. Waiting for messages on stdin...")

    # Initial check for API key, client will be fully initialized on first user message
    if not OPENAI_API_KEY:
        log_message("CRITICAL: OPENAI_API_KEY is not set. AI functionality will be disabled.")
        # No point in sending this via send_response as Pascal might not be ready or listening for errors yet.
        # Pascal side should ideally check for Python readiness or errors after startup.

    # Initialize conversation with a system prompt
    # add_to_conversation("system", "You are a helpful assistant integrated into Cheat Engine. You can use tools to interact with Cheat Engine.")
    # Let's make the system prompt more concise or managed by the user / CE settings later.
    # For now, a simple one.
    conversation_history.append( {"role": "system", "content": "You are a helpful assistant for Cheat Engine."} )


    try:
        for line in sys.stdin:
            line = line.strip()
            if not line:
                continue

            log_message(f"Received raw line: {line}")

            try:
                data = json.loads(line)
                log_message(f"Parsed data: {data}")

                message_type = data.get("message_type")

                if message_type == "user_message":
                    handle_user_message(data)
                elif message_type == "function_call_result":
                    handle_function_call_result(data)
                elif message_type == "system_init": # New message type for Pascal to send tool defs
                    handle_system_init(data)
                else:
                    log_message(f"Unknown message type: {message_type}")
                    send_response({
                        "message_type": "error",
                        "content": f"Python AI: Unknown message type received: {message_type}",
                        "request_id": data.get("request_id", "")
                    })

            except json.JSONDecodeError as e:
                log_message(f"JSONDecodeError: {e}. Raw line: {line}")
                send_response({"message_type": "error", "content": f"Python AI: Invalid JSON received: {line}"})
            except Exception as e:
                log_message(f"General error processing message: {type(e).__name__} - {e}")
                send_response({"message_type": "error", "content": f"Python AI: An internal error occurred: {str(e)}"})

    except KeyboardInterrupt:
        log_message("AI Engine shutting down (KeyboardInterrupt).")
    except Exception as e:
        log_message(f"AI Engine critical error: {e}")
    finally:
        log_message("AI Engine stopped.")

if __name__ == "__main__":
    # Create/clear log file on start for cleaner logs per session, or append if preferred.
    # For now, appending.
    # if os.path.exists(LOG_FILE_PATH):
    #    os.remove(LOG_FILE_PATH)
    main()
