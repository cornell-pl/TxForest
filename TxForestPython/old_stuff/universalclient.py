
import sys


from src.specs import *
from src.forest import Forest


class UniversalClient():
    def __init__(self, path):
        dspec = Directory({
            'files': lambda: RegexComp(
                lambda x: Path(x, File()),
                '[a-zA-Z_0-9]*'
            ),
            'dirs': lambda: RegexComp(
                lambda x: Path(x, dspec.desugar()),
                '[a-zA-Z_0-9]*'
            ),
        })

        spec = dspec.desugar()

        self.forest = Forest(spec, path)

    def ls_pair(self):
        res = self.forest.fetch()
        if isinstance(res, PairRep):
            self.forest.into_pair()
            child = self.forest.fetch()
            if isinstance(child, PathRep):
                child_name = child.get_u()
                self.forest.next()
                to_ret = self.ls_pair().append(child_name)
            else:
                self.forest.next()
                to_ret = self.ls_pair()
            self.forest.out()
            return to_ret
        else:
            return []

    def ls(self):
        res = self.forest.fetch()
        if isinstance(res, DirRep):
            return res.get_contents()
        if isinstance(res, CompRep):
            childrens = []
            num_children = len(res.get_xs())
            self.forest.into_comp()
            for i in range(0, num_children):
                child = self.forest.fetch()
                if isinstance(child, PathRep):
                    childrens.append(child.get_u())
                if not i == num_children - 1:
                    self.forest.next()
            self.forest.out()
            return childrens
        elif isinstance(res, PairRep):
            res = self.ls_pair()
            res.reverse()
            return res
        return []

    def run(self):
        while True:
            next_line = raw_input('> ')

            s = next_line.split()

            cmd = s[0]
            args = s[1:]

            if cmd == 'exit':
                break
            elif cmd == 'down':
                self.forest.general_into()
            elif cmd == 'up':
                self.forest.general_out()
            elif cmd == 'next':
                self.forest.next()
            elif cmd == 'prev':
                self.forest.prev()
            elif cmd == 'fetch':
                res = self.forest.fetch()
                print res
            elif cmd == 'ls':
                print '    '.join(self.ls())
            elif cmd == 'touch':
                file_name = args[0]
                cur_dir = self.forest.fetch().get_lst()
                cur_dir.append(file_name)
                self.forest.store_dir(cur_dir)
            elif cmd == 'update':
                v = args[0]
                self.forest.store_file(v)
            elif cmd == 'commit':
                self.forest.commit()


def run_operation(path):
    UniversalClient(path).run()
