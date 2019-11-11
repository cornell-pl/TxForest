

class Zipper():
  def __init__(self, cur=None, left=[], right=[], anc=None):
    self.cur = cur
    self.left = left
    self.right = right
    self.anc = anc

  def down(self):
    pass

  def up(self):
    return self.anc

  def right(self):
    pass

  def left(self):
    pass


def make_zipper(spec, path):
  pass