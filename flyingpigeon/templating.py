# the templating module is inspired by flask


import os
from jinja2 import Environment, FileSystemLoader

template_path = os.path.join(os.path.abspath(os.path.dirname(__file__)), "templates")

TEMPLATES = Environment(loader=FileSystemLoader(template_path),
                        autoescape=True)


def update_template_context(context):
    """Update the template context with some commonly used variables.
    The original values in the context will not be overridden if a
    context processor decides to return a value with the same key.

    :param context: the context as a dictionary that is updated in place
                    to add extra variables.
    """
    common_vars = dict(static_url='/static')
    orig_ctx = context.copy()
    context.update(common_vars)
    # make sure the original values win.  This makes it possible to
    # easier add new variables in context processors without breaking
    # existing views.
    context.update(orig_ctx)


def render_template(template_name, **context):
    """Renders a template from the template folder with the given
    context.
    :param template_name_or_list: the name of the template to be
                                  rendered.
    :param context: the variables that should be available in the
                    context of the template.
    """
    template = TEMPLATES.get_template('analogviewer.html')
    update_template_context(context)
    return template.render(context)
