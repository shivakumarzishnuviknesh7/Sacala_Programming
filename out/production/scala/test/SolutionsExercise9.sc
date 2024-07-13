
// Task 1
class Author (val firstName: String, val lastName: String)
class BookDescription (val title: String, val authors: List[Author])
class Book (val id: Int, val bookDescription: BookDescription)
class BookManager {
  def addAuthor(a: Author, authors: List[Author]): List[Author] = {
    val list = authors.filter(author => author.firstName == a.firstName && author.lastName == a.lastName)
    if (list.isEmpty) a :: authors
    else authors
  }
  def findAuthor(firstName: String, lastName: String, authors: List[Author]): Option[Author] =
    authors.find(author => author.firstName == firstName && author.lastName == lastName)
  def addBookDescription(bd: BookDescription, bookDescriptions: List[BookDescription]): List[BookDescription] = {
    val list = bookDescriptions.filter(_.title == bd.title)
    if (list.isEmpty) bd :: bookDescriptions
    else bookDescriptions
  }
  def findBookDescription(title: String, bookDescriptions: List[BookDescription]): Option[BookDescription] =
    bookDescriptions.find(_.title == title)
  def addBook(b: Book, books: List[Book]): List[Book] = {
    val list = books.filter(_.id == b.id)
    if (list.isEmpty) b :: books
    else books
  }
  def findBook(id: Int, books: List[Book]): Option[Book] = books.find(_.id == id)
}

val manager = new BookManager
val a1 = Author("Jason", "Swartz")
val a2 = Author("Paul", "Chiusano")
val a3 = Author("Runar", "Bjarnason")
val listAuthors = List(a1, a2, a3)
manager.findAuthor("Jason", "Swartz", listAuthors)
manager.findAuthor("Paul", "Chiusano", listAuthors)
manager.findAuthor("a", "b", listAuthors)
val a4 = Author("Martin", "Odersky")
val listAuthors1 = manager.addAuthor(a4, listAuthors)
manager.findAuthor("Martin", "Odersky", listAuthors1)

val s1 = "Learning Scala"
val bd1 = BookDescription(s1, a1 :: Nil)
val s2 = "Functional Programming in Scala"
val bd2 = BookDescription(s2, List(a2, a3))
val listBookDescriptions = List(bd1, bd2)
manager.findBookDescription(s1, listBookDescriptions)
manager.findBookDescription(s2, listBookDescriptions)
manager.findBookDescription("abc", listBookDescriptions)
val bd3 = BookDescription("Programming in Scala", List(a4))
val listBookDescriptions2 = manager.addBookDescription(bd3, listBookDescriptions)
manager.findBookDescription("Programming in Scala", listBookDescriptions2)

val b1 = new Book(1, bd1)
val b2 = new Book(2, bd1)
val b3 = new Book(3, bd2)
val listBooks = List(b1, b2, b3)
manager.findBook(1, listBooks)
manager.findBook(3, listBooks)
manager.findBook(0, listBooks)
val listBooks2 = manager.addBook(Book(4, bd2), listBooks)
manager.findBook(4, listBooks2)

// Task 2
abstract class Item {
  def price: Double
  def description: String
}
class SimpleItem (val pprice: Double, val pdescription: String) extends Item {
  override def price = pprice
  override def description = pdescription
}
class Bundle (val items: List[Item]) extends Item {
  override def price = items.foldRight(0.0)(_.price + _)
  override def description = items.foldRight("")(_.description + "; " + _)
  def addItem(item: Item) = new Bundle(item :: items)
}
val i1 = new SimpleItem(5.0, "Table leg")
i1.price
i1.description

val i2 = new SimpleItem(30.0, "Table top")
val bi1 = new Bundle(i2 :: List.fill(4)(i1))
bi1.description
bi1.price
val bi2 = bi1.addItem(new SimpleItem(15.0, "Drawer"))
bi2.description
bi2.price



