"""Python preset using ty instead of basedpyright."""


def servers():
    return [["ty", "server"], ["ruff", "server"], ["harper-ls", "-s"]]
