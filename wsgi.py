import sys
import hy

from hypo import hypo_start_wsgi

application = hypo_start_wsgi(sys.argv[1:])
