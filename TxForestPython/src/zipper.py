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
        new_cur = self.l[-1]
        del self.l[-1]
        self.r.insert(0, self.cur)
        self.cur = new_cur

    def right(self):
        new_cur = self.r[0]
        del self.r[0]
        self.l.append(self.cur)
        self.cur = new_cur
