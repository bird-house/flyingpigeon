# the templating module is inspired by flask


import os
from jinja2 import Environment, FileSystemLoader

template_path = os.path.join(os.path.abspath(os.path.dirname(__file__)), "templates")

TEMPLATES = Environment(loader=FileSystemLoader(template_path),
                        autoescape=True)


def render_template(template_name, **context):
    """Renders a template from the template folder with the given
    context.
    :param template_name_or_list: the name of the template to be
                                  rendered.
    :param context: the variables that should be available in the
                    context of the template.
    """
    template = TEMPLATES.get_template('analogviewer.html')
    return template.render(context)
