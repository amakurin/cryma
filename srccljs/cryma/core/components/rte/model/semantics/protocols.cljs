(ns cryma.core.components.rte.model.semantics.protocols)

(defprotocol IGenericHtmlExportSupport
  (html-tag
    [this node conf]
    "Returns root html tag for node.")
  (html-attrs
    [this node conf]
    "Returns root html attrs for node."))


(defprotocol IMaterialSemantics
  (join-nodes
    [this start-node end-node conf]
    "Joins start-node with end-node. Returns vector of one or two nodes.")
  (copy-selection
    [this node start end inverse? conf]
    "Returns content of node from start to end positions which are nillable integers (nil means no boundary). inverse? flag, if specified, means that selection should be inversed (delete selection instead copy).")
  (count-positions
    [this node conf]
    "Returns count of positions for node.")
  (type-in
    [this node position text conf]
    "Returns map of node with text typed in position, and new position .")
  (deny-type-in?
    [this node position text conf]
    "Determines whether typing in should be denied for node.")
  (deny-wrap?
    [this node conf]
    "Determines whether wrapping (e.g. link wrap) in should be denied for node.")
  (to-text
    [this node conf]
    "Renders node to text.")
  (html-content
    [this node conf]
    "Returns html tag content for material node."))

(defprotocol IBlockSemantics
  (html-line-tag
    [this node conf]
    "Returns html tag for child line.")
  (allow-new-line?
    [this block-z line-z conf]
    "Determines whether new line should be denied for block."))

