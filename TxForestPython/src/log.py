
import json

class LogEntry():
  def __init__(self, path):
    self.path = path

  def get_path(self):
    return self.path

  def serialize(self):
    return {}


class ReadFile(LogEntry):
  def __init__(self, path):
    LogEntry.__init__(self, path)

  def serialize(self):
    return {
      'type' : 'R',
      'path': self.path
    }


class WriteFile(LogEntry):
  def __init__(self, path, u):
    LogEntry.__init__(self, path)
    self.u = u

  def serialize(self):
    return {
      'type' : 'WF',
      'path': self.path,
      'contents': self.u
    }

class WriteDir(LogEntry):
  def __init__(self, path, s):
    LogEntry.__init__(self, path)
    self.s = s

  def serialize(self):
    return {
      'type' : 'WD',
      'path': self.path,
      'contents': self.s
    }


def serialize_le(le):
  return le.serialize()

def deserialize_le(le):
  if le['type'] == 'R':
    return ReadFile(le['path'])
  elif le['type'] == 'WF':
    return WriteFile(le['path'], le['contents'])
  elif le['type'] == 'WD':
    return WriteDir(le['path'], le['contents'])



