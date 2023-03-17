import fungus

adtEnum(Tree):
  Branch: tuple[branches: seq[Tree]]
  Leaf: tuple[value: string]

proc toTree(collection: seq[string]): Tree =
  if collection.len == 1:
    result = Tree Leaf.init("")
    discard (var result: Leaf) from result
    result.value.add "."

  else:
    result = Tree Branch.init(@[])
    discard (var result: Branch) from result
    result.branches.add collection[0..<collection.len div 2].toTree
    result.branches.add collection[collection.len div 2..^1].toTree

echo @["Hello", "world", "this", "is", "some", "data", "to", "be", "put", "into", "a", "tree"].toTree
