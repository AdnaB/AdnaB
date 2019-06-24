import sys
import os 
from graphviz import Source

# Get current path
path = os.path.dirname(os.path.realpath(__file__))
# Join current path plus dot file specified in command line
dot_file = sys.argv[1]
if not dot_file.endswith('.dot'):
    dot_file = dot_file + '.dot'
path = os.path.join(path,dot_file)
# Open the dot file and save as .png
s = Source.from_file(path,format='png')
s.view()