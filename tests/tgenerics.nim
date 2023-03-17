import fungus
adtEnum(LinkedList[T]):
  Nil
  Node: tuple[n: ref LinkedList[T], val: T]

proc `$`[T](nl: Nil[T]): string = "nil"

proc `==`[T](a, b: LinkedList[T]): bool = adtEqual(a, b)

proc newNode[T](val: T): Node[T] =
  result = Node[T].init((ref LinkedList[T])(), val)
  result.n[] = LinkedList[T] Nil[T].init()

proc prepend[T](node: LinkedList[T], val: T): LinkedList[T] =
  result = LinkedList[T] Node[T].init(new LinkedList[T], val)
  Node[T](result).n[] = node

proc len[T](list: LinkedList[T]): int =
  var theList = list
  while (node: Node) from theList:
    inc result
    theList = node.n[]

proc `$`[T](list: LinkedList[T]): string =
  var theList = list
  result = "["
  while (node: Node) from theList:
    result.add $node.val
    result.add ", "
    theList = node.n[]

  if result.len > 1:
    result.setLen(result.len - 2)
  result.add "]"

var myList = LinkedList[int] newNode(10)
myList = myList.prepend(11)
myList = myList.prepend(12)
myList = myList.prepend(13)
echo myList
echo myList.len

var myList2 = LinkedList[string] newNode("world")
myList2 = myList2.prepend "cruel"
myList2 = myList2.prepend "hello"
echo myList2
echo myList2.len
echo Nil[int].init()

echo LinkedList[int](Nil[int].init()) == LinkedList[int](Nil[int].init())
