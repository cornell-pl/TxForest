

class Zipper():
  def __init__(self, cur=None, left=[], right=[], anc=None):
    self.cur = cur
    self.l = left
    self.r = right
    self.anc = anc

  def current(self):
    return self.cur


  def ancestor(self):
    return self.anc

  def left(self):
    new_cur = self.left[-1]
    del self.left[-1]
    self.right.insert(0, self.cur)
    self.cur = new_cur

  def right(self):
    new_cur = self.right[0]
    del self.right[0]
    self.left.append(self.cur)
    self.cur = new_cur