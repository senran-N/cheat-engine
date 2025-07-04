# Cheat Engine AI 集成项目 - 优化技术文档

## 📋 项目概述

本文档旨在提供一个优化后的技术方案，用于将AI助手功能集成到Cheat Engine中。AI助手将通过自然语言交互，安全、高效地控制Cheat Engine的核心操作，包括内存扫描、数值修改、调试器控制等。

## 🔧 技术方案选择

**核心架构：异步进程通信与事件驱动**

- **前端UI (Lazarus/Pascal)**：负责用户界面、Cheat Engine核心功能调用、以及与Python进程的通信管理。
- **AI引擎 (Python)**：独立进程，负责AI模型的调用、自然语言处理、Function Calling解析，并向Pascal端发送操作指令。
- **通信桥梁**：采用基于标准输入/输出流（stdin/stdout）的JSON-RPC（或自定义JSON协议）进行异步通信，辅以文件系统或命名管道进行更复杂的数据传输（如大块内存数据）。
- **CE操作**：所有对Cheat Engine的实际操作仍由Pascal端执行，确保性能、稳定性和兼容性。

## 🎯 核心目标

- **完全控制**：AI能够执行用户在 Cheat Engine 中可以进行的所有操作。
- **自然交互**：用户通过自然语言与AI对话，AI能够理解并执行复杂的游戏修改任务。
- **响应迅速**：UI不因AI处理或进程间通信而阻塞，保持流畅的用户体验。
- **安全稳定**：严格控制AI对CE操作的权限，避免潜在的风险。
- **开放兼容**：支持OpenAI兼容的任何AI模型和服务。

## 🧠 通信机制与线程分配

### 1. 通信协议：基于JSON的异步消息传递

- **请求/响应模式**：Pascal端向Python端发送请求（用户输入、CE状态查询），Python端处理后返回响应（AI回复、Function Call指令）。
- **JSON格式**：所有通信数据均采用JSON格式，便于解析和扩展。
- **消息类型**：定义明确的消息类型，例如：
    - `user_message`: 用户输入的自然语言。
    - `ce_status_request`: 请求CE当前状态（如当前进程、扫描结果）。
    - `ai_response`: AI的自然语言回复。
    - `function_call_request`: AI解析出的Function Call指令（函数名、参数）。
    - `function_call_result`: Pascal端执行Function Call后的结果。
    - `error_message`: 错误信息。

### 2. 进程通信方式：标准I/O流 + 辅助通道

- **主通信通道 (stdin/stdout)**：
    - **Pascal -> Python**: Pascal端将JSON请求写入Python进程的stdin。
    - **Python -> Pascal**: Python进程将JSON响应写入其stdout。
    - **优点**: 简单、高效、跨平台兼容性好。
    - **挑战**: 需要处理好缓冲、编码（UTF-8）和长消息的传输。
- **辅助通信通道 (可选，用于大数据)**：
    - **命名管道 (Named Pipes)** 或 **临时文件**: 当需要传输大量数据（如完整的进程列表、大块内存数据）时，可以通过主通道协商，然后使用命名管道或临时文件进行传输，避免阻塞stdin/stdout。
    - **优点**: 提高大数据传输效率，不影响主通信通道的实时性。

### 3. 线程分配与异步处理

#### Pascal 端 (Cheat Engine UI)

- **主UI线程**：负责界面的渲染和用户交互，**绝不执行阻塞操作**。
- **Python通信管理线程 (Dedicated Thread)**：
    - 负责启动和管理Python子进程。
    - 异步读取Python进程的stdout，并将接收到的JSON消息放入一个线程安全的队列。
    - 异步写入JSON请求到Python进程的stdin。
    - **优点**: 将进程通信的阻塞操作从UI线程中分离，确保UI的流畅性。
- **CE操作执行线程 (Dedicated Thread Pool)**：
    - 当Python端返回`function_call_request`时，将CE操作请求放入一个工作队列。
    - 一个或多个工作线程从队列中取出请求，执行实际的Cheat Engine操作（内存扫描、读写等）。
    - 操作完成后，将结果封装为`function_call_result`消息，通过通信管理线程发送回Python。
    - **优点**: 避免CE操作阻塞UI，支持并发操作（如果CE API允许）。
- **事件驱动机制**：
    - Pascal端通过事件（如自定义消息、回调）通知UI线程更新界面或处理AI响应。
    - 例如，当通信管理线程接收到AI回复时，触发一个UI更新事件，UI线程安全地更新聊天窗口。

#### Python 端 (AI Engine)

- **主事件循环/线程**：
    - 负责监听stdin的输入。
    - 解析JSON请求。
    - 调用AI模型（OpenAI API请求通常是异步的）。
    - 处理Function Calling逻辑。
    - 将JSON响应写入stdout。
- **AI模型调用**：利用`asyncio`或线程池来处理对OpenAI API的异步请求，避免阻塞Python进程。
- **CE操作桥接**：Python端不直接执行CE操作，而是将Function Call指令序列化为JSON，通过stdout发送给Pascal端。

## 🏗️ AI 如何操控 Cheat Engine

AI对Cheat Engine的操控将通过**Function Calling**机制实现，但关键在于**AI本身不执行任何操作**。

1. **函数定义 (Pascal -> Python)**：
    - Pascal端维护一个Cheat Engine可执行操作的函数列表（例如：`open_process`, `memory_scan`, `write_memory`等）。
    - 这些函数定义（包括名称、描述、参数的JSON Schema）在Python进程启动时，或者通过一个初始化消息，传递给Python AI引擎。
    - Python AI引擎将这些定义作为`functions`参数传递给OpenAI API。

2. **用户请求 (Pascal -> Python)**：
    - 用户在Cheat Engine的AI窗口输入自然语言请求（例如：“帮我把游戏里的金币改成999”）。
    - Pascal端将此请求封装为`user_message` JSON，通过stdin发送给Python进程。

3. **AI解析与指令生成 (Python)**：
    - Python AI引擎接收到用户请求后，将其与对话历史、以及Pascal端提供的Function Definitions一起发送给OpenAI模型。
    - OpenAI模型分析用户意图，如果认为需要执行某个CE操作，它将返回一个`function_call`对象，包含函数名和参数。
    - Python AI引擎接收到`function_call`后，将其封装为`function_call_request` JSON，通过stdout发送回Pascal端。

4. **CE操作执行 (Pascal)**：
    - Pascal端的通信管理线程接收到`function_call_request`。
    - 将此请求转发给CE操作执行线程池。
    - 工作线程根据指令调用Cheat Engine内部对应的Pascal函数（例如：`TMemScan.firstScan`，`WriteProcessMemory`）。
    - **安全检查**: 在执行任何敏感操作前，Pascal端可以进行额外的安全检查和用户确认（例如，弹窗询问用户是否允许修改内存）。

5. **结果反馈与对话延续 (Pascal -> Python -> AI)**：
    - CE操作执行完成后，Pascal端将操作结果（成功/失败、返回数据）封装为`function_call_result` JSON，通过stdin发送回Python进程。
    - Python AI引擎接收到结果后，将其作为`role: function`消息添加到对话历史中，并再次调用OpenAI模型。
    - OpenAI模型根据操作结果生成自然语言回复（例如：“金币已成功修改为999”），并将其作为`ai_response` JSON发送回Pascal端。
    - Pascal端更新AI聊天窗口，完成一次交互循环。

## 📁 最终项目结构 (优化后)

```
Cheat Engine/
├── AI/                           # AI 模块 (Pascal UI & Communication)
│   ├── AIAssistantUnit.pas       # AI助手主窗口 (UI, 事件处理)
│   ├── AIConfigUnit.pas          # AI配置对话框
│   ├── AIPythonBridge.pas        # Python 进程通信管理 (线程安全队列, I/O处理)
│   ├── CEOperationExecutor.pas   # CE操作执行器 (线程池, 封装CE核心API)
│   ├── AIFunctionDefinitions.pas # AI可调用函数定义 (JSON Schema生成)
│   └── AIConversationManager.pas # 对话状态管理 (可选，用于复杂多轮对话)
├── AI_Python/                    # Python AI 引擎
│   ├── ai_engine.py              # 核心AI引擎脚本 (stdin/stdout解析, OpenAI调用, Function Calling处理)
│   ├── ce_api_interface.py       # CE API接口定义 (Python端对Pascal端CE操作的抽象)
│   ├── requirements.txt          # Python依赖
│   └── setup_ai.bat              # 环境安装脚本
├── images/ai/                    # AI相关图标资源
└── [现有文件...]
```

## 🎯 关键技术要点总结 (优化后)

### 1. 异步非阻塞通信
- **Pascal端**: 专用线程处理Python进程的I/O，确保UI响应。
- **Python端**: 利用异步库处理OpenAI API调用，避免阻塞。

### 2. 严格的职责分离
- **Pascal**: UI、CE核心操作、安全控制、进程管理。
- **Python**: AI逻辑、自然语言处理、Function Calling解析。

### 3. 安全与权限控制
- AI只生成指令，实际执行由Pascal端严格控制。
- Pascal端可在执行敏感操作前加入用户确认机制。

### 4. 错误处理与日志
- 完善的JSON错误消息传递机制。
- 详细的日志记录，便于调试和问题追踪。

### 5. 可扩展性
- 新的CE功能只需在Pascal端封装，并在Function Definitions中添加即可。
- Python AI引擎可独立更新，不影响Cheat Engine主程序。
