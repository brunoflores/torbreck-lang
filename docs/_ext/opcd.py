from docutils import nodes

from docutils.parsers.rst import directives
from docutils.parsers.rst import Directive

import re

# An op code with an optional argument in brackets.
op_regex = re.compile(r"(?P<op>\w+)(?P<par>\(.*\))?")
# An argument with an optional subscript looking ahead for an
# optional comma (the separator).
arg_regex = re.compile(r"(?P<arg>[a-zA-Z01\.]+)(?P<sub>\s{1}[\d+k]+)?(?=,?)")


def opcode_role(role, rawtext, text, lineno, inliner, options=None, content=None):
    def parse_op(op):
        def parse_arg(arg):
            if not arg.group("sub"):
                return arg.group("arg")
            else:
                return "%s_{%s}" % (arg.group("arg"), arg.group("sub"))

        m = op_regex.match(op)
        if not m:
            return ""
        if not m.group("par"):
            return r"\textbf{%s}" % m.group("op")
        else:
            args = [parse_arg(arg) for arg in re.finditer(arg_regex, m.group("par"))]
            return r"\textbf{%s}(%s)" % (m.group("op"), ", \space ".join(args))

    node = nodes.math(text="; \space".join([parse_op(op) for op in text.split("; ")]))
    return [node], []


def setup(app):
    app.add_role("opcode", opcode_role)

    return {
        "version": "0.1",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
