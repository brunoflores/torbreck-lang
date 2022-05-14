from docutils import nodes

from docutils.parsers.rst import directives
from docutils.parsers.rst import Directive

import re

# Pre-compile.
opcode_role_regex = re.compile(
    "(?P<op>\w+)(?P<par>\((?P<arg>[a-zA-Z]+)(?P<sub>\d+)?\))?"
)


class OpCode(Directive):
    required_arguments = 1
    optional_arguments = 0
    has_content = False
    node_class = nodes.math

    option_spec = {
        "op": directives.unchanged_required,
    }

    def run(self):
        text = r"\textbf{" + self.arguments[0] + "}(arg_{8})"
        math_node = self.node_class(text=text)
        self.state.nested_parse(self.content, self.content_offset, math_node)
        return [math_node]


def opcode_role(role, rawtext, text, lineno, inliner, options=None, content=None):
    final = []
    ops = text.split("; ")
    for op in ops:
        m = opcode_role_regex.match(op)
        if not m:
            continue
        if not m.group("par"):
            final.append(r"\textbf{{{op}}}".format(op=m.group("op")))
        elif not m.group("sub"):
            final.append(
                r"\textbf{{{op}}}({arg})".format(op=m.group("op"), arg=m.group("arg"))
            )
        else:
            final.append(
                r"\textbf{{{op}}}({arg}_{{{sub}}})".format(
                    op=m.group("op"), arg=m.group("arg"), sub=m.group("sub")
                )
            )
    node = nodes.math(text="; ".join(final))
    return [node], []


def setup(app):
    app.setup_extension("sphinx.ext.mathjax")
    app.add_directive("opcode", OpCode)
    app.add_role("opcode", opcode_role)

    return {
        "version": "0.1",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
