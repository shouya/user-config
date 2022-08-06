import subprocess

def pass_show(name):
  args = ["pass", "show", name]
  try:
    return subprocess.check_output(args).strip()
  except subprocess.CalledProcessError:
    return ""
