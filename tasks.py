from invoke import task
from traceback import print_exc

import logging
log = logging.getLogger()

@task
def build(c, docs=False, hide=False):
    do(c, hide=hide)


def do(c, count=0, hide=False):
    if count <= 100:
        try:
            result = c.run("nix build .#wk01174", hide=hide)
            print("done with " + str(count) + " try")
        except Exception as e:
            print("retry")
            do(c, count=count+1)

    else:
        print("sad")
