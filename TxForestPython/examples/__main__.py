import sys

import index
import universalclient


example = sys.argv[1]
args = sys.argv[2:]
if example == 'index':
  path = args[0]
  op = int(args[1])
  args = args[2:]

  index.run_operation(path, op, args)

elif example == 'universalclient':
  path = args[0]
  universalclient.run_operation(path)