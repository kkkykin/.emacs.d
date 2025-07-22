#!/usr/bin/env uv run -s
import subprocess
import sys
import os
from threading import Thread

def parse_script_args(args):
  """
  解析脚本专用参数（--encoding, --timeout 等），返回配置字典和命令参数列表
  格式：python run_command.py [--encoding ENCODING] [--timeout SECONDS] [--] <command> [args...]
  """
  config = {
    "encoding": "utf-8",  # 默认编码
    "timeout": None,     # 默认无超时
  }
  command_args = args.copy()
  split_index = len(args)  # 初始值（无 -- 时）

  # 查找 -- 分隔符
  if "--" in args:
    split_index = args.index("--")
    command_args = args[split_index + 1:]  # -- 之后是命令
    args_to_parse = args[:split_index]   # -- 之前是脚本参数
  else:
    args_to_parse = args.copy()

  # 解析脚本参数（--encoding, --timeout 等）
  i = 0
  while i < len(args_to_parse):
    arg = args_to_parse[i]
    if arg == "--encoding" and i + 1 < len(args_to_parse):
      config["encoding"] = args_to_parse[i + 1]
      i += 2
    elif arg == "--timeout" and i + 1 < len(args_to_parse):
      config["timeout"] = float(args_to_parse[i + 1])
      i += 2
    else:
      i += 1

  return config, command_args

def stream_output(pipe, print_func, encoding="utf-8"):
  """实时打印子进程的输出流（处理编码）"""
  try:
    for raw_line in iter(pipe.readline, b''):
      try:
        line = raw_line.decode(encoding, errors="replace")
        print_func(line, end="")
      except Exception as e:
        print_func(f"\n[DecodeError] {e}\n", end="", file=sys.stderr)
  except BrokenPipeError:
    pass

def main():
  if len(sys.argv) < 2:
    print("Usage: python run_command.py [--encoding ENCODING] [--timeout SECONDS] [--] <command> [args...]")
    print("Example 1: python run_command.py ping baidu.com")
    print("Example 2: python run_command.py --encoding gbk --timeout 5 -- ping -c 4 baidu.com")
    sys.exit(1)

  # 解析参数
  config, command_args = parse_script_args(sys.argv[1:])
  if not command_args:
    print("Error: No command specified", file=sys.stderr)
    sys.exit(1)

  try:
    # 启动子进程
    env = {**os.environ, "PYTHONIOENCODING": "utf-8"}
    process = subprocess.Popen(
      command_args,
      stdin=subprocess.DEVNULL,
      stdout=subprocess.PIPE,
      stderr=subprocess.PIPE,
      bufsize=0,
      env=env,
    )

    # 启动输出线程
    stdout_thread = Thread(
      target=stream_output,
      args=(process.stdout, print, config["encoding"]),
    )
    stderr_thread = Thread(
      target=stream_output,
      args=(process.stderr, lambda x, **kwargs: print(x, **kwargs, file=sys.stderr), config["encoding"]),
    )
    stdout_thread.start()
    stderr_thread.start()

    # 等待子进程结束（带超时控制）
    process.wait(timeout=config["timeout"])
    stdout_thread.join()
    stderr_thread.join()

    if process.returncode != 0:
      print(f"\nCommand failed with code: {process.returncode}", file=sys.stderr)

  except subprocess.TimeoutExpired:
    print(f"\n[Timeout] Command exceeded {config['timeout']} seconds", file=sys.stderr)
    process.terminate()
  except KeyboardInterrupt:
    print("\n[Interrupted]", file=sys.stderr)
    process.terminate()
  except Exception as e:
    print(f"\n[Error] {e}", file=sys.stderr)

if __name__ == "__main__":
  main()
