package week1

object generators {

  trait Generator[+T] {
    self => // an alias for ”this”.

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }

  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }                                               //> integers  : week1.generators.Generator[Int]{val rand: java.util.Random} = we
                                                  //| ek1.generators$$anonfun$main$1$$anon$3@28ba21f3
  val booleans = for (x <- integers) yield x > 0  //> booleans  : week1.generators.Generator[Boolean] = week1.generators$Generator
                                                  //| $$anon$1@30c7da1e

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)                                  //> pairs: [T, U](t: week1.generators.Generator[T], u: week1.generators.Generato
                                                  //| r[U])week1.generators.Generator[(T, U)]

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }                                               //> single: [T](x: T)week1.generators.Generator[T]

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)  //> choose: (lo: Int, hi: Int)week1.generators.Generator[Int]

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)
                                                  //> oneOf: [T](xs: T*)week1.generators.Generator[T]
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list                                    //> lists: => week1.generators.Generator[List[Int]]

  def emptyLists = single(Nil)                    //> emptyLists: => week1.generators.Generator[scala.collection.immutable.Nil.ty
                                                  //| pe]

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail                            //> nonEmptyLists: => week1.generators.Generator[List[Int]]

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree                                    //> trees: => week1.generators.Generator[week1.generators.Tree]

  def leafs = for (x <- integers) yield Leaf(x)   //> leafs: => week1.generators.Generator[week1.generators.Leaf]

  def inners = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)                      //> inners: => week1.generators.Generator[week1.generators.Inner]

  trees.generate                                  //> res0: week1.generators.Tree = Inner(Leaf(543833373),Inner(Leaf(845660258),I
                                                  //| nner(Leaf(-351698154),Leaf(-1079170310))))
}