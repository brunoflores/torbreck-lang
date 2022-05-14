import os
import sys

# Add the _ext directory to the Python path. This is for custom extensions.
# https://www.sphinx-doc.org/en/master/development/tutorials/helloworld.html
sys.path.append(os.path.abspath("./_ext"))

# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = "Torbreck"
copyright = "2022, Bruno Flores"
author = "Bruno Flores"


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
#
# See https://www.sphinx-doc.org/en/master/usage/extensions/index.html
extensions = ["sphinx.ext.mathjax", "sphinx.ext.graphviz", "opcd"]

# See https://docs.mathjax.org/en/latest/input/tex/extensions.html
mathjax3_config = {
    "loader": {"load": ["[tex]/textmacros", "[tex]/textcomp"]},
    "tex": {
        "packages": {"[+]": ["textmacros"]},
        "autoload": {"textcomp": ["textlbrackdbl", "textrbrackdbl"]},
    },
    "textmacros": {"packages": {"[+]": ["textcomp"]}},
}

# Add any paths that contain templates here, relative to this directory.
templates_path = ["_templates"]

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store", ".venv"]


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = "sphinx_rtd_theme"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ["_static"]
