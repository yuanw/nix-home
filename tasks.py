from invoke import task


@task
def build(c, docs=False):
    result = c.run("nix build .#wk01174")
    print(result.ok)
    print(result.stdout.splitlines()[-1])
