
import socket
from threading import Thread
import json

from log import *
from globalforest import GlobalForest

class ClientHandler(Thread):
  def __init__(self, conn, gf):
    Thread.__init__(self)
    print 'client connected'
    self.globalforest = gf

    self.conn = conn
    self.buffer = ''

  def _try_recieve(self):
    try:
      return self.conn.recv(1024, socket.MSG_DONTWAIT)
    except:
      return ""

  def _receive_msg(self):
    self.buffer = self.buffer + self._try_recieve()
    if (not self.buffer == '') and '-' in self.buffer:
      s = self.buffer.split('-')
      msg_len = int(s[0])
      rest = '-'.join(s[1:])
      if len(rest) >= msg_len:
        #recived a full msg
        msg = rest[:msg_len]
        self.buffer = rest[msg_len:]
        return msg
      else:
        return ''
    else:
      return ''

  def _prepend_length_to_msg(self, msg):
    msg_len = len(msg)
    return str(msg_len) + '-' + msg

  def _send_msg(self, msg):
    try:
      msg = self._prepend_length_to_msg(msg)
      self.conn.send(msg)
    except e, Exception:
      print 'exn: ' + e

  def _parse_log(self, raw_log):
    json_log = json.loads(raw_log)
    return [deserialize_le(json_le) for json_le in json_log]

  def run(self):
    while True:
      data = self._receive_msg()
      if not data == '':
        print 'received msg: ' + data
        s = data.split()
        cmd = s[0]
        rest = ' '.join(s[1:])
        if cmd == 'commit':
          print 'commit requested'
          log = self._parse_log(rest)
          can_commit = self.globalforest.commit(log)
          resp = 'commit_resp' + ' ' + json.dumps(can_commit)
          self._send_msg(resp)
          print 'succeeded' if can_commit else 'failed'
        elif cmd == 'finish_commit':
          print 'commit finished'
          self.globalforest.finish_commit()
          resp = 'finish_commit_resp' + ' ' + json.dumps(True)
          self._send_msg(resp)
        else:
          print 'unsuported msg: ' + data


class ForestServer():
  def __init__(self):
    self.globalforest = GlobalForest()

    self.host = 'localhost'
    self.port = 22222
    self.listening_connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    self.listening_connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    self.listening_connection.bind((self.host, self.port))


  def run(self):
    self.listening_connection.listen(100)
    while True:
      conn, addr = self.listening_connection.accept()
      ch = ClientHandler(conn, self.globalforest)
      ch.start()


if __name__ == '__main__':
  server = ForestServer()
  server.run()



