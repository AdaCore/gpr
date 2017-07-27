class TreeWalkerFrame:
    def __init__(self, Node, SubNodeList, Index):
        self.node      = Node
        self.node_list = SubNodeList
        self.i         = Index

class TreeWalker:
    """Walking a directed graph (a tree) without relying on recursion of the
       programming language.

       (C) 2011 Frank-Rene Schaefer

       TERMINOLOGY ____________________________________________________________

                                .------.
                           .--->| node |
                           |    '......'
                    .------.    .------.    
                --->| node |--->| node |    
                    '......'    '......'    
                           |    .------.    .------.
                           '--->| node |--->| node |
                                '......'    '......'

       Node:                An element of the directed graph. A 'node' is 
                            indicated as a rectangle in the figure.

       Directed relation:   Is a relationship with a definite direction between
                            two nodes of the graph. A 'directed relation' is 
                            shown as an arrow.
                          
       Sub nodes of a node: A list of nodes where for each 'sub-node' in the 
                            list it holds that 'sub-node' has a directed 
                            relation from 'node' to 'sub-node', i.e.

                                       node --------> sub-node

       Data that is organized by a nodes and sub nodes is called a 'directed 
       graph' or a 'tree'.
                           
       EXPLANATION ____________________________________________________________

       A tree walker is an object that can walk along data which is organized
       as a tree without relying on recursion. It can be used by deriving from
       it and implementing the following member functions:

         .on_enter(node):    -- handle event of 'node' being entered
                             -- return list of sub nodes or 'None'. 

                                If 'None' is returned the node is refused.
                                Then, no 'on_finished' will be called for it.

                                If for the same node the 'on_finished' is 
                                desired, return an empty list.
                             
         .on_finished(node): -- handling the event of all sub nodes
                                of 'node' being treated. 

       The walk is entered by a call to function 

                             .do(RootNode)

       or  

                             .do(RootNodeList)

       This function walks down the tree in 'depth-first' and calls the member
       functions upon their defined events. When the function returns the 
       whole tree has been covered.

       Comments:

       -- As long as no 'parallel state' is involved, the only requirement
          for the algorithm is that the user provides a list of sub nodes
          by function 'on_enter()'.

       -- If there is a 'parallel state' is involved it must be synchronized
          by both 'on_enter()' and 'on_finished()' events. An example is 
          the current working directory, if one is walking along a file
          system tree. Then 'on_enter()' must contain a 'change_to_directory()'
          and 'on_finished()' must contain a 'change_to_root_directory()'.

       -- Leaf node detection can be done by the user when there are no sub 
          nodes to be returned by 'on_enter()'.

       ________________________________________________________________________
       HINT: 

       print "# %s ..." % (self.depth * " ", ...)

       provides nicely indented print-outs for debugging

       CYCLIC GRAPHS __________________________________________________________
       
       A cyclic graph can stall the algorithm below. To avoid entering loops an
       infinite amount of times, the function 

                             on_enter(node)

       shall return None as soon as a loop has been detected. The node is then
       refused. No 'on_finished' will be called for it, which would complicate
       the handling of the path stack and other parallel state variables. An
       easy way to do so is to keep track of the path, and detect whether the
       node already appears in the walked down path. That is, 

           def on_enter(self, node):       
               if node in self.path: return None # -- Cylce detected, 
               #                                 #    refuse to dive deeper
               self.path.append(node)            # -- Mark node on path
               return get_sub_node_list(node)    #    get sub node list for diving

           def on_finished(self, node):    
               self.path.pop()

       STATE __________________________________________________________________

       The example of a '.path' member of TreeWalker insinuates the importance 
       of the concept of the TreeWalker's state. Here, the 'state' keeps track
       of the currently walked down path. Any other data related to the data
       that is walked along may be stored as members of the (derived) TreeWalker 
       object.

       For debug purposes, the '.depth' property can be used to probe the depth 
       of the current recursion.
    """
    def __init__(self):
        self.abort_f = False

    def do(self, InitNode_OrInitList):
        if type(InitNode_OrInitList) == list: init_node_list = InitNode_OrInitList 
        else:                                 init_node_list = [ InitNode_OrInitList ]

        frame = TreeWalkerFrame(None, init_node_list, -1)
        self.work_stack = []
        while 1 + 1 == 2:
            frame.i += 1
            node          = frame.node_list[frame.i]
            sub_node_list = self.on_enter(node)
            if self.abort_f: return

            if (sub_node_list is not None) and len(sub_node_list) != 0:
                self.work_stack.append(frame)                      # branch node
                frame = TreeWalkerFrame(node, sub_node_list, -1) 
            else:
                if sub_node_list is not None:                 # is node not totally refused?
                    self.on_finished(node)                    # no sub nodes
                while frame.i == len(frame.node_list) - 1:    # last in row    
                    if len(self.work_stack) == 0:    
                        return
                    frame = self.work_stack.pop()
                    self.on_finished(frame.node_list[frame.i])
                    if frame.i != len(frame.node_list) - 1: 
                        break
    @property
    def depth(self):
        return len(self.work_stack)

    def on_enter(self, node):     assert False # --> user's derived class
    def on_finished(self, node):  assert False #           - " -

if __name__ == "__main__":
    # Example: Some tree about regions in the world and beyond. 
    class Some:
        def __init__(self, Name):
            self.name   = Name
            self.places = None
    a = [Some("World"), Some("Mars") ]
    a[0].places = [ Some("Asia"), Some("Europe"), Some("Afrika") ]
    a[0].places[0].places = [ Some("China"), Some("Japan"), Some("Thailand") ]
    a[0].places[1].places = [ Some("France"), Some("Germany"), Some("Poland"), Some("Italy") ]
    a[0].places[1].places[1].places = [ Some("Schwaben"), Some("Baden") ]
    a[0].places[1].places[1].places[0].places = [ Some("Stuttgart"), Some("Ulm") ]
    a[0].places[1].places[1].places[1].places = [ Some("Karlsruhe"), Some("Rastadt") ]
    a[0].places[2].places = [ Some("Morocco"), Some("Egypt"), Some("Nigeria"), Some("Kongo") ]
    a[1].places = [ Some("A") ]
    a[1].places[0].places = [ Some("AB") ]
    a[1].places[0].places[0].places = [ Some("ABC") ]

    class DirectoryTreeWalker(TreeWalker):
        def __init__(self):  
            self.path = []
            self.depth = 0
            TreeWalker.__init__(self)

        def on_enter(self, node):  
            self.depth += 1
            print "    " * self.depth + " + " + node.name
            self.path.append(node.name)
            if node.places is None: return []
            # print repr(self.path)[1:-1]
            return node.places

        def on_finished(self, node):   
            print "    " * self.depth + " - " + node.name
            self.depth -= 1
            self.path.pop()
    
    DirectoryTreeWalker().do(a)

