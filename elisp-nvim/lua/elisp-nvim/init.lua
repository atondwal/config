-- Elisp interpreter bridge for Neovim
-- Starts headless Emacs daemon and communicates via JSON-RPC

local M = {}
local uv = vim.loop

-- State
M.emacs_job = nil
M.emacs_stdin = nil
M.emacs_stdout = nil
M.emacs_pid = nil
M.request_id = 0
M.pending_requests = {}
M.buffer_map = {} -- nvim bufnr -> emacs buffer name
M.reverse_buffer_map = {} -- emacs buffer name -> nvim bufnr

-- Configuration
M.config = {
  emacs_path = "emacs",
  startup_elisp = vim.fn.stdpath('config') .. '/elisp-nvim/elisp/nvim-bridge.el',
  debug = true,
}

-- Logging
local function log(msg)
  if M.config.debug then
    print("[elisp-nvim] " .. msg)
  end
end

local function log_error(msg)
  vim.notify("[elisp-nvim] ERROR: " .. msg, vim.log.levels.ERROR)
end

-- JSON-RPC message handling
local function send_request(method, params)
  if not M.emacs_stdin then
    log_error("Emacs not running")
    return nil
  end

  M.request_id = M.request_id + 1
  local id = M.request_id

  local request = {
    jsonrpc = "2.0",
    id = id,
    method = method,
    params = params or {}
  }

  local json_str = vim.json.encode(request) .. "\n"
  log("Sending request: " .. method)

  -- Store callback
  local result_future = {}
  M.pending_requests[id] = result_future

  -- Send to emacs
  M.emacs_stdin:write(json_str)

  return id, result_future
end

local function send_notification(method, params)
  if not M.emacs_stdin then
    log_error("Emacs not running")
    return
  end

  local notification = {
    jsonrpc = "2.0",
    method = method,
    params = params or {}
  }

  local json_str = vim.json.encode(notification) .. "\n"
  M.emacs_stdin:write(json_str)
end

local function handle_response(response)
  local id = response.id
  if not id then
    log_error("Response without id")
    return
  end

  local future = M.pending_requests[id]
  if not future then
    log_error("Unknown request id: " .. id)
    return
  end

  M.pending_requests[id] = nil

  if response.error then
    log_error("RPC error: " .. vim.inspect(response.error))
    future.error = response.error
  else
    future.result = response.result
  end

  future.done = true
end

local function handle_request(request)
  -- Handle requests from Emacs to manipulate Neovim
  local method = request.method
  local params = request.params or {}
  local id = request.id

  log("Received request: " .. method)

  -- Dispatch to handler
  local handler = M.rpc_handlers[method]
  if not handler then
    local error_response = {
      jsonrpc = "2.0",
      id = id,
      error = {
        code = -32601,
        message = "Method not found: " .. method
      }
    }
    M.emacs_stdin:write(vim.json.encode(error_response) .. "\n")
    return
  end

  -- Execute handler
  local success, result = pcall(handler, params)

  local response = {
    jsonrpc = "2.0",
    id = id,
  }

  if success then
    response.result = result
  else
    response.error = {
      code = -32603,
      message = tostring(result)
    }
  end

  M.emacs_stdin:write(vim.json.encode(response) .. "\n")
end

-- RPC method handlers (called by Emacs)
M.rpc_handlers = {}

function M.rpc_handlers.nvim_get_current_buffer(params)
  local bufnr = vim.api.nvim_get_current_buf()
  local bufname = M.buffer_map[bufnr] or vim.api.nvim_buf_get_name(bufnr)
  return {buffer = bufnr, name = bufname}
end

function M.rpc_handlers.nvim_buf_get_lines(params)
  local bufnr = params.buffer or vim.api.nvim_get_current_buf()
  local start = params.start or 0
  local end_line = params["end"] or -1
  local lines = vim.api.nvim_buf_get_lines(bufnr, start, end_line, false)
  return {lines = lines}
end

function M.rpc_handlers.nvim_buf_set_lines(params)
  local bufnr = params.buffer or vim.api.nvim_get_current_buf()
  local start = params.start or 0
  local end_line = params["end"] or -1
  local lines = params.lines or {}
  vim.api.nvim_buf_set_lines(bufnr, start, end_line, false, lines)
  return {success = true}
end

function M.rpc_handlers.nvim_buf_get_name(params)
  local bufnr = params.buffer or vim.api.nvim_get_current_buf()
  local name = vim.api.nvim_buf_get_name(bufnr)
  return {name = name}
end

function M.rpc_handlers.nvim_buf_set_name(params)
  local bufnr = params.buffer or vim.api.nvim_get_current_buf()
  local name = params.name
  vim.api.nvim_buf_set_name(bufnr, name)
  M.buffer_map[bufnr] = name
  M.reverse_buffer_map[name] = bufnr
  return {success = true}
end

function M.rpc_handlers.nvim_command(params)
  vim.cmd(params.command)
  return {success = true}
end

function M.rpc_handlers.nvim_echo(params)
  print(params.message)
  return {success = true}
end

-- Parse JSON-RPC messages from stdout
local stdout_buffer = ""

local function process_stdout(data)
  stdout_buffer = stdout_buffer .. data

  -- Process complete lines
  while true do
    local newline_pos = stdout_buffer:find("\n")
    if not newline_pos then break end

    local line = stdout_buffer:sub(1, newline_pos - 1)
    stdout_buffer = stdout_buffer:sub(newline_pos + 1)

    if line:match("^%s*$") then
      goto continue
    end

    -- Try to parse JSON
    local success, json = pcall(vim.json.decode, line)
    if not success then
      log_error("Failed to parse JSON: " .. line)
      goto continue
    end

    -- Dispatch based on message type
    if json.method then
      if json.id then
        -- Request from Emacs
        vim.schedule(function() handle_request(json) end)
      else
        -- Notification from Emacs
        log("Notification: " .. json.method)
      end
    elseif json.id then
      -- Response to our request
      handle_response(json)
    end

    ::continue::
  end
end

-- Start Emacs daemon
function M.start_emacs()
  if M.emacs_job then
    log("Emacs already running")
    return
  end

  log("Starting Emacs daemon...")

  local stdin = uv.new_pipe(false)
  local stdout = uv.new_pipe(false)
  local stderr = uv.new_pipe(false)

  local handle, pid = uv.spawn(M.config.emacs_path, {
    args = {
      "--batch",
      "--load", M.config.startup_elisp,
      "--eval", "(nvim-bridge-start-server)"
    },
    stdio = {stdin, stdout, stderr}
  }, function(code, signal)
    log("Emacs exited with code " .. code)
    M.emacs_job = nil
    M.emacs_stdin = nil
    M.emacs_stdout = nil
    M.emacs_pid = nil
  end)

  if not handle then
    log_error("Failed to start Emacs: " .. tostring(pid))
    return false
  end

  M.emacs_job = handle
  M.emacs_pid = pid
  M.emacs_stdin = stdin
  M.emacs_stdout = stdout

  -- Read stdout
  stdout:read_start(function(err, data)
    if err then
      log_error("stdout error: " .. tostring(err))
    elseif data then
      process_stdout(data)
    end
  end)

  -- Read stderr for debugging
  stderr:read_start(function(err, data)
    if err then
      log_error("stderr error: " .. tostring(err))
    elseif data then
      log("Emacs stderr: " .. data)
    end
  end)

  log("Emacs daemon started (pid: " .. pid .. ")")
  return true
end

function M.stop_emacs()
  if not M.emacs_job then
    return
  end

  log("Stopping Emacs daemon...")

  -- Send kill request
  send_notification("kill-emacs", {})

  -- Force kill after timeout
  vim.defer_fn(function()
    if M.emacs_job then
      M.emacs_job:kill(15) -- SIGTERM
    end
  end, 1000)
end

-- Public API for evaluating elisp
function M.eval(elisp_code, callback)
  local id, future = send_request("eval", {code = elisp_code})

  if callback then
    -- Poll for result
    local function check_result()
      if future.done then
        callback(future.result, future.error)
      else
        vim.defer_fn(check_result, 10)
      end
    end
    check_result()
  end

  return future
end

-- Execute elisp interactively
function M.eval_interactive()
  vim.ui.input({prompt = "Elisp: "}, function(code)
    if not code then return end

    M.eval(code, function(result, error)
      if error then
        vim.notify("Error: " .. vim.inspect(error), vim.log.levels.ERROR)
      else
        vim.notify("Result: " .. vim.inspect(result), vim.log.levels.INFO)
      end
    end)
  end)
end

-- Setup function
function M.setup(opts)
  M.config = vim.tbl_deep_extend("force", M.config, opts or {})

  -- Create commands
  vim.api.nvim_create_user_command("ElispStart", function()
    M.start_emacs()
  end, {})

  vim.api.nvim_create_user_command("ElispStop", function()
    M.stop_emacs()
  end, {})

  vim.api.nvim_create_user_command("ElispEval", function(opts)
    if opts.args and opts.args ~= "" then
      M.eval(opts.args, function(result, error)
        if error then
          vim.notify("Error: " .. vim.inspect(error), vim.log.levels.ERROR)
        else
          vim.notify("Result: " .. vim.inspect(result), vim.log.levels.INFO)
        end
      end)
    else
      M.eval_interactive()
    end
  end, {nargs = "?"})

  vim.api.nvim_create_user_command("ElispDired", function(opts)
    local path = opts.args ~= "" and opts.args or vim.fn.getcwd()
    M.eval('(dired "' .. path .. '")', function(result, error)
      if error then
        vim.notify("Dired error: " .. vim.inspect(error), vim.log.levels.ERROR)
      end
    end)
  end, {nargs = "?", complete = "dir"})

  -- Auto-start on first use
  vim.api.nvim_create_autocmd("VimEnter", {
    callback = function()
      if M.config.auto_start then
        M.start_emacs()
      end
    end
  })

  -- Cleanup on exit
  vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
      M.stop_emacs()
    end
  })
end

return M
