Readme

To run Triplets:
- install haskell compiler
- go to "Triplets folder"
- run Triplets.hs
- call main

To make a graph from the .dot file:
- install graphivz to path and via pip for python
- run "python dot_to_graph.py graph
- the graph will be created as "graph.dot.png"

To run Quartets (not finished):
- install haskell compiler
- go to "Quartets - not finished" folder
- run quartets.hs
- initiate a model with quar_1 = quar_init(.,.,.,.,.,.,.,.) (fill in the deal here as seen in "main" in Triplets)
- do update by calling: quar_2 = ask_card old_model asking_player asked_player card to update the model with a move
