# Cheat Engine AI 集成开发指南 (V2)

本文档是 `Cheat_Engine_AI_Integration_Documentation.md` 的补充和具体实现指南，旨在指导开发者完成AI助手与Cheat Engine主程序的代码级集成。

## 1. 最终项目结构 (建议)

为了保持代码的模块化和清晰，建议采用以下目录结构。这与初始文档基本一致，但更具体化。

```
Cheat Engine/
├── AI/                           # AI 模块 (Pascal UI & Communication)
│   ├── AIAssistantFrame.pas      # AI助手界面单元 (TFrame, 可嵌入主窗口)
│   ├── AIAssistantFrame.lfm      # 界面布局文件
│   ├── AI.Core/                  # 核心逻辑
│   │   ├── AI.Bridge.pas         # (原AIPythonBridge) Python进程与通信管理
│   │   ├── AI.Executor.pas       # (原CEOperationExecutor) CE操作的调用封装
│   │   └── AI.Functions.pas      # (原AIFunctionDefinitions) AI可调用函数的定义与注册
│   └── AI.SharedTypes.pas        # 共享数据结构 (JSON序列化/反序列化)
├── AI_Python/                    # Python AI 引擎 (保持不变)
│   ├── ai_engine.py
│   ├── requirements.txt
│   └── ...
└── ... (现有文件)
```

## 2. UI 开发 (`AIAssistantFrame.pas`)

1.  **组件选择**: 使用 `TFrame` 而不是 `TForm` 来创建 AI 助手界面。`TFrame` 可以方便地嵌入到 Cheat Engine 的主窗口或其他窗口中，提供更原生的体验。
2.  **界面元素**:
    *   一个 `TRichEdit` 或 `TSynEdit` 用于显示对话历史。
    *   一个 `TEdit` 或 `TMemo` 用于用户输入。
    *   一个 `TButton` 用于发送消息。
    *   一个 `TStatusBar` 或 `TLabel` 用于显示 AI 的当前状态 (例如：“正在思考...”, “正在执行扫描...”, “准备就绪”)。
3.  **事件处理**:
    *   **OnEnter (输入框)** / **OnClick (按钮)**:
        - 将输入框中的文本打包成 `user_message` JSON。
        - 调用 `AI.Bridge` 单元中的 `SendMessageToPython(jsonString)`。
        - 禁用输入框，更新状态为“正在思考...”。
    *   **UI 更新**: UI 的更新**不应**在任何工作线程中直接调用。`AI.Bridge` 应该使用 `TThread.Synchronize` 或 `TThread.Queue` 来将从 Python 收到的消息安全地传递给主UI线程进行显示。

## 3. 通信与线程 (`AI.Bridge.pas`)

这是连接 Pascal 和 Python 的桥梁，必须在独立线程中运行，以防阻塞UI。

*   **`TAIPythonBridge` 类**:
    *   **成员**:
        *   `FPythonProcess: TProcess`: 用于启动和管理 `python.exe`。
        *   `FReaderThread: TThread`: 专用的stdout读取线程。
        *   `FRequestQueue: TThreadedQueue<string>`: 线程安全的队列，用于存放待发送给 Python 的消息。
        *   `FWriterThread: TThread`: 专用的stdin写入线程。
    *   **`Start` 方法**:
        1.  配置 `TProcess` 的参数，包括 Python 解释器路径、`ai_engine.py` 脚本路径。
        2.  设置 `TProcess.Options` 包括 `poUsePipes`, `poNoConsole`, `poNewHidden`。
        3.  启动 `FWriterThread` 和 `FReaderThread`。
        4.  执行 `FPythonProcess.Execute`。
    *   **`Stop` 方法**:
        1.  优雅地终止线程和进程。
        2.  向 Python 发送一个 "shutdown" 消息，或者直接 `FPythonProcess.Terminate`。
    *   **`SendMessageToPython(jsonString: string)` 方法**:
        - 将消息字符串放入 `FRequestQueue`。
    *   **`FReaderThread.Execute` 方法**:
        - 循环中阻塞读取 `FPythonProcess.Output` (stdout)。
        - 读取到一行或一个完整的JSON块后，调用一个事件或回调（如 `OnMessageReceived`），将数据传递给 `AIAssistantFrame` 进行处理。
    *   **`FWriterThread.Execute` 方法**:
        - 循环中从 `FRequestQueue` 中取出消息 (带超时的 `PopItem`)。
        - 将消息写入 `FPythonProcess.Input` (stdin)，并确保刷新缓冲区 (`Flush`)。

## 4. Pascal 功能封装 (`AI.Functions.pas` & `AI.Executor.pas`)

这是整个项目的核心，它将 Cheat Engine 的强大功能暴露给 AI。Pascal 端需要定义一系列函数，这些函数将由 `AI.Executor.pas` 实现，并在 `AI.Functions.pas` 中注册，最终以 JSON Schema 的形式提供给 Python AI 引擎。

所有耗时操作（如扫描、附加进程）都应异步执行，并立即返回一个任务ID。AI 可以使用此ID来轮询任务状态和获取最终结果。

---

### **A. 进程管理**

| 函数名 | 描述 | 参数 | 返回值 |
| :--- | :--- | :--- | :--- |
| `listProcesses` | 获取当前正在运行的进程列表。 | (无) | `{"status": "success", "processes": [{"pid": 123, "name": "game.exe"}, ...]}` |
| `openProcess` | 根据进程ID打开一个进程，用于后续操作。 | `{"pid": 123}` | `{"status": "success", "message": "成功附加到 game.exe"}` |
| `getAttachedProcess` | 获取当前 Cheat Engine 附加到的进程信息。 | (无) | `{"status": "success", "process": {"pid": 123, "name": "game.exe"}}` 或 `null` |

---

### **B. 内存扫描**

| 函数名 | 描述 | 参数 | 返回值 |
| :--- | :--- | :--- | :--- |
| `startFirstScan` | 启动一次新的首次内存扫描。这是一个异步操作。 | `{"value": "100", "type": "4 Bytes", "scan_option": "Exact Value", "start_address": "0x00000000", "end_address": "0x7FFFFFFF", "protection_flags": "PAGE_READWRITE"}` | `{"status": "success", "task_id": "scan_1"}` |
| `startNextScan` | 在上次扫描结果的基础上执行再次扫描。异步操作。 | `{"value": "120", "scan_option": "Increased Value", "task_id": "scan_1"}` | `{"status": "success", "task_id": "scan_1"}` |
| `startAOBScan` | 启动一次AOB（字节数组）扫描。异步操作。 | `{"aob": "41 42 43 ?? 45", "scan_option": "aob", "start_address": "...", "end_address": "..."}` | `{"status": "success", "task_id": "scan_2"}` |
| `getScanStatus` | 查询一个扫描任务的状态。 | `{"task_id": "scan_1"}` | `{"status": "running", "progress": 0.75}` 或 `{"status": "completed", "found_count": 50}` |
| `getScanResults` | 从已完成的扫描任务中获取结果。 | `{"task_id": "scan_1", "offset": 0, "count": 20}` | `{"status": "success", "results": ["0x123456", "0xABCDEF", ...]}` |
| `clearScan` | 清除所有扫描结果，重置扫描状态。 | `{"task_id": "scan_1"}` | `{"status": "success"}` |

---

### **C. 地址列表与作弊表**

| 函数名 | 描述 | 参数 | 返回值 |
| :--- | :--- | :--- | :--- |
| `addAddressToCheatTable` | 将一个地址添加到主界面的地址列表（作弊表）中。 | `{"address": "0x123456", "type": "4 Bytes", "description": "金币地址"}` | `{"status": "success", "index": 0}` |
| `getCheatTableEntries` | 获取当前作弊表中的所有条目。 | (无) | `{"status": "success", "entries": [{"index": 0, "address": "...", "description": "...", "value": "100"}, ...]}` |
| `setCheatTableEntryValue` | 修改作弊表中指定条目的内存数值。**（需用户确认）** | `{"index": 0, "value": "999"}` | `{"status": "success"}` |
| `setCheatTableEntryDescription` | 修改作弊表中指定条目的描述。 | `{"index": 0, "description": "修改后的金币"}` | `{"status": "success"}` |
| `toggleFreezeAddress` | 切换作弊表中指定条目的数值冻结状态。 | `{"index": 0, "freeze": true}` | `{"status": "success"}` |
| `removeCheatTableEntry` | 从作弊表中移除一个条目。 | `{"index": 0}` | `{"status": "success"}` |
| `loadCheatTable` | 从文件加载一个作弊表。 | `{"path": "C:\\path\\to\\table.CT"}` | `{"status": "success"}` |
| `saveCheatTable` | 将当前作弊表保存到文件。 | `{"path": "C:\\path\\to\\table.CT"}` | `{"status": "success"}` |


---

### **D. 调试与分析**

| 函数名 | 描述 | 参数 | 返回值 |
| :--- | :--- | :--- | :--- |
| `startFindWhatAccesses` | 启动一个监视器，找出是哪些代码在访问一个特定地址。异步。**（需用户确认）** | `{"address": "0x123456"}` | `{"status": "success", "task_id": "fwwa_1"}` |
| `startFindWhatWrites` | 启动一个监视器，找出是哪些代码在写入一个特定地址。异步。**（需用户确认）** | `{"address": "0x123456"}` | `{"status": "success", "task_id": "fwww_1"}` |
| `getFindResults` | 获取监视任务的结果（即触发断点的指令列表）。 | `{"task_id": "fwwa_1"}` | `{"status": "success", "hits": [{"address": "0x401000", "instruction": "MOV EAX, [0x123456]"}, ...]}` |
| `stopFindTask` | 停止一个正在运行的监视任务。 | `{"task_id": "fwwa_1"}` | `{"status": "success"}` |
| `getDisassembly` | 获取指定地址附近的反汇编代码。 | `{"address": "0x401000", "lines": 20}` | `{"status": "success", "disassembly": ["0x401000: MOV EAX, ...", ...]}` |
| `setBreakpoint` | 在指定地址设置一个软件断点。**（需用户确认）** | `{"address": "0x401000", "enabled": true}` | `{"status": "success", "breakpoint_id": "bp_1"}` |
| `getBreakpointContext` | 当断点命中时，获取当前的CPU寄存器和堆栈信息。 | `{"breakpoint_id": "bp_1"}` | `{"status": "success", "context": {"EAX": "...", "EBX": "...", "stack": [...]}}` |
| `removeBreakpoint` | 移除一个已设置的断点。 | `{"breakpoint_id": "bp_1"}` | `{"status": "success"}` |

---

### **E. 指针扫描**

| 函数名 | 描述 | 参数 | 返回值 |
| :--- | :--- | :--- | :--- |
| `startPointerScan` | 对一个地址启动指针扫描。这是一个非常耗时的异步操作。**（需用户确认）** | `{"target_address": "0x123456", "max_level": 5, "max_offset": 2048}` | `{"status": "success", "task_id": "pscan_1"}` |
| `getPointerScanResults` | 获取指针扫描的结果。 | `{"task_id": "pscan_1"}` | `{"status": "success", "pointers": [["base+10", "0x20", "0x30"], ...]}` |

---

### **F. 脚本与高风险操作**

**警告：** 以下功能具有高风险，可能导致目标进程或系统不稳定。默认应禁用，且每次执行都必须经过用户弹窗二次确认。

| 函数名 | 描述 | 参数 | 返回值 |
| :--- | :--- | :--- | :--- |
| `directReadMemory` | 直接从任意内存地址读取数据。 | `{"address": "0xABCDEF", "type": "Byte", "count": 16}` | `{"status": "success", "data": "41 42 43..."}` |
| `directWriteMemory` | 直接向任意内存地址写入数据，绕过作弊表。 | `{"address": "0xABCDEF", "type": "Byte", "value": "255"}` | `{"status": "success"}` |
| `assembleAndInject` | 在指定地址汇编并注入一小段机器码。 | `{"address": "0x401000", "code": "MOV EAX, 1\nNOP\nRET"}` | `{"status": "success", "bytes_written": 6}` |
| `executeLuaScript` | 执行一段Lua脚本。这是最强大的功能，也最危险。 | `{"script": "writeFloat(0x123456, 100.0)"}` | `{"status": "success", "output": "...", "error": "..."}` |

---

### **G. 系统工具**

| 函数名 | 描述 | 参数 | 返回值 |
| :--- | :--- | :--- | :--- |
| `setSpeedhack` | 设置全局速度修改器的速度。 | `{"speed": 2.0}` | `{"status": "success"}` |
| `getSpeedhack` | 获取当前速度修改器的速度。 | (无) | `{"status": "success", "speed": 2.0}` |

## 5. 安全性考虑

*   **用户确认**: 对于所有写入或注入操作 (例如 `setCheatTableEntryValue`, `startFindWhatAccesses`, `directWriteMemory`, `assembleAndInject`, `setBreakpoint`, `executeLuaScript`)，在 `AI.Executor` 的实现中，必须弹出一个对话框向用户请求确认。
*   **功能开关**: 在 AI 设置中，提供复选框，允许用户完全禁用某些高风险功能（如代码注入、内核模式操作）。 