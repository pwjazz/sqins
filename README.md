sqins - (s)tructured (q)ueries (in) (S)cala
===========================================

sqins makes SQL INSERT, UPDATE, DELETE and SELECT statements available inside Scala.  sqins makes 80% of typical DML
really easy and stays out of the way for the other 20%.

**License** - [BSD 3 Clause](http://www.opensource.org/licenses/BSD-3-Clause)

**Status** - Alpha

**Author** - Percy Wegmann

### Benefits

 * Looks like SQL - if you know SQL, you pretty much know sqins.
 * Strongly typed - compile time checking for SQL syntax and column types.
 * Not an ORM - sqins is just a SQL API.
 * Simple mapping - unlike an ORM, sqins just needs to know the most basic things about your tables and columns.
 * Extensible scalar types - sqins comes with support for basic types like numbers, strings and dates and makes it super-easy to define new type mappings.
 * No mutable state - sqins doesn't cache data, maintain identity or relationships or do any other ORM funkiness.  This makes it easier to use, and makes it usable in idiomatic Scala. 
 * Smart performance - sqins employs common-sense optimizations like using PreparedStatements, streaming results from cursors, etc.
 * No SQL-injection - because sqins uses PreparedStatements and bound parameters, sqins makes it hard (though not impossible) to introduce SQL-injection vulnerabilities.
 * Works with PostgreSQL - other databases are on the way.
 * Runs in your container - sqins doesn't manage its own database connections or transactions, so it plays nice with any container or even no container.

### A quick example

````scala
import java.sql._
import org.sqins._
import org.sqins.Implicits._

// Define our Scala model (needs to be case classes)
case class Invoice(id: Long = -1,
                   description: String,
                   image: Option[Array[Byte]] = None)

case class LineItem(id: Long = -1,
                    invoice_id: Long,
                    amount: BigDecimal,
                    ts: Timestamp = new Timestamp(System.currentTimeMillis()) {
  // Set up a query directly inside our object (ActiveRecord anyone?)
  private val line_item = new LineItemTable()

  def insert = INSERT INTO (line_item) VALUES (this)
}

// Define our database tables
// Tables have two type parameters, the row type and the primary key type
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
}

// Set up names for our tables
val invoice = new InvoiceTable()
val line_item = new LineItemTable()

// Get a Connection
val conn: Connection = getConnection // get a connection however you like

// Set up some queries
val newInvoice = Invoice(description = "An invoice")
val query = INSERT INTO invoice VALUES (newInvoice)

// Insert an invoice and some line items 
val newInvoiceId:Option[Long] = query(conn) 
newInvoiceId.map { id =>
  for (i <- 1 to 5) yield
    LineItem(invoice_id = id, amount = i).insert(conn)
}

// Make the connection implicit so we don't have to keep using it explicitly
implicit val implicitConn = conn

// Update the amount on all of our line items
// Notice the use of ?() to bind the value 50
UPDATE(line_item) SET (line_item.amount := ?(50)) go

<<<<<<< HEAD
// You can also do a whole object update if you want
val updateLineItem = LineItem(id = 1, invoice_id = 1, amount = 50)
UPDATE(line_item) SET (line_item) go

=======
>>>>>>> 3e395ac491d93e4c1aab1f5fb41a82dc7f874582
// Set up some aliases for our tables
val i = invoice AS "i"
val li = line_item AS "li"

// Query for invoices with line items
val selectQuery = (
  SELECT (i.*, li.*)
  FROM (i INNER_JOIN li ON i.id == li.invoice_id)
  ORDER_BY (i.id, li.ts DESC))
  
// Print out our query expression just to see what it looks like
println(selectQuery.expression)

// Run the query
val selectResult = selectQuery go

// Iterate through the results and print the info
selectResult.foreach(row => {
  println("Invoice Id: " + row._1.id)
  println("Invoice Description: " + row._1.description)
  println("Line Item Id: " + row._2.id)
  println("Line Item Amount: " + row._2.amount)
  println("Line Item Timestamp: " + row._2.ts)
})

// Use FN([string]) for your database's functions
val queryWithFunctions = (
  SELECT (i.*, FN("my_function")(li.amount))
  FROM (i INNER_JOIN li ON i.id == li.invoice_id))

// Use EXPR([string]) to plug in scalar expressions not natively supported by sqins
val complicatedSelectQuery = (
  SELECT (i.*, li.*)
  FROM (i INNER_JOIN li ON i.id == li.invoice_id)
  WHERE i.id == EXPR("(SELECT MAX(invoice_id) FROM line_item)")
  ORDER_BY (i.id, li.ts DESC))
  
// Delete the line items and then the invoice
DELETE FROM li go;
DELETE FROM i go
<<<<<<< HEAD

// You can also run arbitrary SQL like this
SQL("""put some really complicated sql in here""")(conn)
=======
>>>>>>> 3e395ac491d93e4c1aab1f5fb41a82dc7f874582
````

For an extended example, take a look at core_tests.scala.

### Supported SQL Features

#### INSERT queries

````
INSERT INTO table [ ( column [, ...] ) ]
    VALUES ( expression [, ...] )
````

#### UPDATE queries

````
UPDATE table [ [ AS ] alias ]
<<<<<<< HEAD
    SET {{ column = expression } [,...] | row }
=======
    SET { column = expression } [,...]
>>>>>>> 3e395ac491d93e4c1aab1f5fb41a82dc7f874582
          ( column [, ...] ) = ( { expression | DEFAULT } [, ...] ) } [, ...]
````

#### DELETE queries

````
DELETE FROM table [ [ AS ] alias ]
    [ WHERE condition ]
````

#### SELECT queries

````
SELECT [ DISTINCT ] ]
    * | expression [ [ AS ] output_name ] [, ...]
    [ FROM from_item [, ...] ]
    [ WHERE condition ]
    [ GROUP BY expression [, ...] ]
    [ HAVING condition [, ...] ]
    [ ORDER BY expression [ ASC | DESC ] [, ...] ]
    [ LIMIT { count } ]
    [ OFFSET start [ ROW ] ]
````

### Roadmap

* All queries
    * Support for correlated subqueries in expressions
    * Support for correlated subqueries in conditions 
    * Support for mapping to non-case classes???
* INSERT queries
    * Support for using a subquery in lieu of the VALUES clause
    * Support for DEFAULT column values in the VALUES clause    
* SELECT queries
    * Support for UNION, INTERSECT and EXCEPT
* Database Support
    * MySQL
    * Oracle
    * SqlServer

### Setup in SBT

TODO: