
import socket
import json

from log import *

class ForestClient():
  def __init__(self):
    self.host = 'localhost'
    self.port = 20001
    self.conn = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    self.conn.connect()
    self.buffer = ''

  def _prepend_length_to_msg(self, msg):
    msg_len = len(msg)
    return str(msg_len) + '-' + msg

  def _send_msg(self, msg):
    try:
      msg = self._prepend_length_to_msg(msg)
      self.conn.send(msg)
    except e, Exception:
      print 'client exn: ' + e

  def _receive_msg(self):
    self.buffer = self.buffer + conn.recv(1024, MSG_DONTWAIT)
    if not self.buffer == '' and '-' in self.buffer:
      s = self.buffer.split('-')
      msg_len = int(s[0])
      rest = s[1:].join('-')
      if len(rest) > msg_len:
        #recived a full msg
        msg = rest[:msg_len]
        self.buffer = rest[msg_len:]
        return msg
      else:
        return ''
    else:
      return ''

  def _wait_for_resp(self):
    while True:
      msg = self._receive_msg()
      if not msg == '':
        return msg


  def send_commit(self, log):
    json_log = [serialize_le(le) for le in log]
    string_log = json.dumps(json_log)
    msg = 'commit ' + ' ' + string_log
    self._send_msg(string_log)

    resp = self._wait_for_resp()
    s = resp.split()
    cmd = s[0]
    if cmd == 'commit_resp':
      resp = ' '.join(s[1:])
      return json.parse(resp)
    else:
      print 'commit: server sent the wrong thing'
      return False


  def send_finish_commit(self):
    msg = 'finish_commit'
    self._send_msg(string_log)

    resp = self._wait_for_resp()
    s = resp.split()
    cmd = s[0]
    if cmd == 'finish_commit_resp':
      resp = ' '.join(s[1:])
      return json.parse(resp)
    else:
      print 'finish_commit: server sent the wrong thing'
      return False



