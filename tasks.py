from invoke import task


@task
def build(c, docs=False):
    result = c.run("nix build .#wk01174", hide=True)
    print(result.exited)
