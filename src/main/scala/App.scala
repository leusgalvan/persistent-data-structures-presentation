object App {
  private final val BRANCHING_FACTOR = 32

  trait HAMT[K, +V]
  case class EmptyHAMT[K, V]() extends HAMT[K, V] {
    override def toString(): String = "_"
  }
  case class Leaf[K, V](key: K, value: V) extends HAMT[K, V]
  case class Many[K, V](children: Array[HAMT[K, V]]) extends HAMT[K, V] {
    override def toString(): String = {
      s"Many(Array(${children.mkString(", ")}))"
    }
  }

  def insert[K, V](_hamt: HAMT[K, V], _key: K, _value: V): HAMT[K, V] = {
    def insert0[K, V](hamt: HAMT[K, V], key: K, value: V, level: Int): HAMT[K, V] = hamt match {
      case EmptyHAMT() => Leaf(key, value)
      case leaf2@Leaf(key2, _) =>
        val index1 = (key.hashCode() >>> level) & 0x1f
        val index2 = (key2.hashCode() >>> level) & 0x1f
        val children: Array[HAMT[K, V]] = Array.fill(BRANCHING_FACTOR) {
          EmptyHAMT()
        }
        children(index1) = Leaf(key, value)
        children(index2) = leaf2
        Many(children)
      case Many(children) =>
        val index1 = (key.hashCode() >>> level) & 0x1f
        children(index1) match {
          case EmptyHAMT() =>
            val newChildren = new Array[HAMT[K, V]](BRANCHING_FACTOR)
            Array.copy(children, 0, newChildren, 0, children.length)
            newChildren(index1) = Leaf(key, value)
            Many(newChildren)

          case hamt2 =>
            val newChildren = new Array[HAMT[K, V]](BRANCHING_FACTOR)
            Array.copy(children, 0, newChildren, 0, children.length)
            newChildren(index1) = insert0(hamt2, key, value, level + 5)
            Many(newChildren)
        }
    }
    insert0(_hamt, _key, _value, 0)
  }

  def find[K, V](_hamt: HAMT[K, V], _key: K): Option[V] = {
    def find0[K, V](hamt: HAMT[K, V], key: K, level: Int): Option[V] = hamt match {
      case EmptyHAMT() => None
      case Leaf(key2, value) => if(key == key2) Some(value) else None
      case Many(children) =>
        val index = (key.hashCode() >>> level) & 0x1f
        find0(children(index), key, level + 5)
    }
    find0(_hamt, _key, 0)
  }
}
