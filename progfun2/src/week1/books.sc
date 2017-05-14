package week1

object books {
  case class Book(title: String, authors: List[String])

  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
                                                  //> books  : List[week1.books.Book] = List(Book(Structure and Interpretation of 
                                                  //| Computer Programs,List(Abelson, Harald, Sussman, Gerald J.)), Book(Introduct
                                                  //| ion to Functional Programming,List(Bird, Richard, Wadler, Phil)), Book(Effec
                                                  //| tive Java,List(Bloch, Joshua)), Book(Effective Java 2,List(Bloch, Joshua)), 
                                                  //| Book(Java Puzzlers,List(Bloch, Joshua, Gafter, Neal)), Book(Programming in S
                                                  //| cala,List(Odersky, Martin, Spoon, Lex, Venners, Bill)))

  for {
    b <- books
    a <- b.authors
    if a startsWith "Bird,"
  } yield b.title                                 //> res0: List[String] = List(Introduction to Functional Programming)

  books flatMap (b =>
    b.authors.withFilter(a =>
      a startsWith "Bird,") map (a => b.title))   //> res1: List[String] = List(Introduction to Functional Programming)

  for {
    b <- books
    if (b.title indexOf "Program") >= 0
  } yield b.title                                 //> res2: List[String] = List(Structure and Interpretation of Computer Programs,
                                                  //|  Introduction to Functional Programming, Programming in Scala)

  for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res3: List[String] = List(Bloch, Joshua, Bloch, Joshua, Bloch, Joshua)

  val bookSet = books.toSet                       //> bookSet  : scala.collection.immutable.Set[week1.books.Book] = Set(Book(Effe
                                                  //| ctive Java 2,List(Bloch, Joshua)), Book(Programming in Scala,List(Odersky, 
                                                  //| Martin, Spoon, Lex, Venners, Bill)), Book(Structure and Interpretation of C
                                                  //| omputer Programs,List(Abelson, Harald, Sussman, Gerald J.)), Book(Effective
                                                  //|  Java,List(Bloch, Joshua)), Book(Introduction to Functional Programming,Lis
                                                  //| t(Bird, Richard, Wadler, Phil)), Book(Java Puzzlers,List(Bloch, Joshua, Gaf
                                                  //| ter, Neal)))

  for {
    b1 <- bookSet
    b2 <- bookSet
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res4: scala.collection.immutable.Set[String] = Set(Bloch, Joshua)

}