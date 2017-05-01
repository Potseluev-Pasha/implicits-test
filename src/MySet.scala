trait MySet[T] {
  def add(t: T): Unit

  def contains(t: T): Boolean
}