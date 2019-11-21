
from threading import Lock


class GlobalForest:
  def __init__(self):
    self.global_log = []
    self.global_lock = Lock()


  def _conflict_path(self, path):
    for le in self.global_log:
      if not isinstance(le, ReadFile):
        #TODO: does this work correctly
        if commonpath([le.get_path, path]) == path:
          return True
    return False

  def _extract_paths(self, log):
    return [le.get_path() for le in log]

  def commit(self, log):
    self.global_lock.acquire()
    paths = self._extract_paths(log)
    for path in paths:
      if self._conflict_path(path): return False
    return True

  def finish_commit(self):
    self.global_lock.release()
