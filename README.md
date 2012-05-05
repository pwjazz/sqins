sqins (structured queries in Scala)
===================================

sqins makes SQL INSERT, UPDATE, DELETE and SELECT statements available inside Scala.  sqins makes 80% of typical DML
really easy and stays out of the way for the other 20%.

**License** - [BSD 3 Clause](http://www.opensource.org/licenses/BSD-3-Clause)

**Status** - Alpha

**Author** - Percy Wegmann

 * Not an ORM - sqins is just a SQL API
 * Strongly typed - compile time checking for SQL syntax and column types
 * Syntax very similar to SQL - if you know SQL, you know sqins
 * Simple object mapping - unlike an ORM, sqins just needs to know the most basic things about your tables and columns
 * Extensible scalar types - sqins comes with support for basic types like numbers, strings and dates and makes it super-easy to define new type mappings
 * Stateless - sqins doesn't cache data, maintain identity or relationships or do any other ORM funkiness
 * Smart performance - sqnis employs common-sense optimizations like using PreparedStatements, streaming results from cursors, etc.
 * Tested with Postgres - other databases on the way

### A quick example

````scala
// Define our Scala model
case class Invoice(id: Long = -1,
                   description: String)

case class LineItem(id: Long = -1,
                    invoice_id: Long,
                    amount: BigDecimal,
                    ts: Timestamp = new Timestamp(System.currentTimeMillis)) {
  // We can set up queries directly inside our objects if we want (ActiveRecord pattern)
  private val line_item = new LineItemTable()

  def insert = INSERT INTO (line_item) VALUES (this)
}

// Define our database tables
class InvoiceTable extends Table[Invoice, Long]("invoice") {
  val id = Column[Long]("id").autoGenerated
  val description = Column[String]("description")

  primaryKey(id)
  columns(id, description)
}

class LineItemTable extends Table[LineItem, Long]("line_item") {
  val id = Column[Long]("id").autoGenerated
  val invoice_id = Column[Long]("invoice_id")
  val amount = Column[BigDecimal]("amount")
  val ts = Column[Timestamp]("ts").autoGenerated

  primaryKey(id)
  columns(id, invoice_id, amount, ts)
} // Set up names for our tables
val invoice = new InvoiceTable()
val line_item = new LineItemTable()

initSchema()

// Set up a connection
val conn: Connection = getConnection // get your connection from wherever you like

// We can set up queries inline
val query = INSERT INTO invoice VALUES (Invoice(description = "An invoice"))

// Let's insert an invoice and some line items 
query(conn) match {
  // If the insert succeeds, we get back the inserted id
  case Some(invoiceId: Long) => {
    for (i <- 1 to 5) yield LineItem(invoice_id = invoiceId, amount = i).insert(conn)
  }
  case None => // ignore
}

// Let's update the amount on all of our line items
(UPDATE(line_item) SET (line_item.amount := ?(50)))(conn)

// Let's query for invoices with line items, and let's use aliases while we're at it
val i = invoice AS "i"
val li = line_item AS "li"

val selectQuery = (
  SELECT(i.*, li.*)
  FROM (i INNER_JOIN li ON i.id == li.invoice_id)
  ORDER_BY (i.id, li.ts DESC))

val selectResult = selectQuery(conn)

// Iterate through the results and print the info
selectResult.foreach(row => {
  println("Invoice Id: " + row._1.id)
  println("Invoice Description: " + row._1.description)
  println("Line Item Id: " + row._2.id)
  println("Line Item Amount: " + row._2.amount)
  println("Line Item Timestamp: " + row._2.ts)
})

// Delete the line items and then the invoice
(DELETE FROM li)(conn)
(DELETE FROM i)(conn)
````

For an extended example, take a look at core_tests.scala.

### Setup in SBT

TODO:
