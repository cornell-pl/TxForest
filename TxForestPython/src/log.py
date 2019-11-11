

class LogEntry():
  def __init__(self, path):
    self.path = path


class ReadFile(LogEntry):
  def __init__(self, path):
    LogEntry.__init__(self, path)


class WriteFile(LogEntry):
  def __init__(self, path, u):
    LogEntry.__init__(self, path)
    self.u = u

class WriteDir(LogEntry):
  def __init__(self, path, s):
    LogEntry.__init__(self, path)
    self.s = s