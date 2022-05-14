from docutils import nodes

from docutils.parsers.rst import directives
from docutils.parsers.rst import Directive

import re

# An op code with an optional argument in brackets.
op_regex = re.compile(r"(?P<op>\w+)(?P<par>\(.*\))?")
# An argument with an optional subscript looking ahead for an
# optional comma (the separator).
arg_regex = re.compile(r"(?P<arg>[a-zA-Z01\.]+)(?P<sub>\s{1}[\d+k]+)?(?=,?)")


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
    app.setup_extension("sphinx.ext.mathjax")
    app.add_directive("opcode", OpCode)
    app.add_role("opcode", opcode_role)

    return {
        "version": "0.1",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
