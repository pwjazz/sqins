sqins - (s)tructured (q)ueries (in) (S)cala
===========================================

sqins provides type-safe SQL INSERT, UPDATE, DELETE and SELECT statements inside Scala.  sqins makes 80% of typical DML
really easy and stays out of the way for the other 20%.

**License** - [BSD 3 Clause](http://www.opensource.org/licenses/BSD-3-Clause)

**Status** - Alpha

**Author** - Percy Wegmann

## This is valid sqins

````scala
case class Invoice(id: Long = -1,
                   description: String,
                   image: Option[Array[Byte]] = None)

case class LineItem(id: Long = -1,
                    invoice_id: Long,
                    amount: BigDecimal,
                    ts: Timestamp = new Timestamp(System.currentTimeMillis())

// Assume that the entities Invoice and LineItem have been mapped by InvoiceTable and LineItemTable

val i = new InvoiceTable() AS "i"
val li = new LineItemTable() AS "li"

db.inTransaction { implicit conn: Connection =>
  INSERT INTO i(i.description) VALUES (?("A new invoice")) map { invoiceId =>
    val newLineItem = LineItem(invoice_id = invoiceId, amount = 25)
  }
}

db.withConnection { implicit conn: Connection =>
  val query = (
    SELECT(i.*, li.*)
    FROM (i INNER_JOIN li ON i.id == li.invoice_id)
    WHERE (i.description == ?("A new invoice")))
    
  query foreach { row =>
        println(row._1.id)
        println(row._1.description)
        println(row._2.id)
        println(row._2.invoice_id)
        println(row._2.amount)
        println(row._2.ts)
    }
}
````

## Benefits

 * Looks like SQL - if you know SQL, you pretty much know sqins.
 * Strongly typed - compile time checking for SQL syntax and column types.
 * Not an ORM - sqins is just a SQL API.
 * Simple mapping - unlike an ORM, sqins just needs to know the most basic things about your tables and columns.
 * Extensible scalar types - sqins comes with support for basic types like numbers, strings and dates and makes it super-easy to define new type mappings.
 * Side-effect free - sqins doesn't cache data, maintain identity or relationships or do any other ORM funkiness.  This makes it easier to use, and makes it usable in idiomatic Scala. 
 * Smart performance - sqins employs common-sense optimizations like using PreparedStatements, streaming results from cursors, etc.
 * No SQL-injection - because sqins uses PreparedStatements and bound parameters, sqins makes it hard (though not impossible) to introduce SQL-injection vulnerabilities.
 * Works with PostgreSQL - other databases are on the way.
 * Runs in your container - sqins doesn't manage its own database connections or transactions, so it plays nice with any container or even no container.

## Installation

// TODO: publish sqins to a Maven repo

## Imports

You'll typically use these 3 imports.

````scala
import java.sql._
import org.sqins._
import org.sqins.Implicits._
````

## Mapping

### Example Database Schema

````sql
DROP TABLE IF EXISTS line_item;
DROP TABLE IF EXISTS invoice;

create table invoice (
  id SERIAL,
  description VARCHAR(255),
  image BYTEA,
  primary key(id));

create table line_item (
  id SERIAL,
  invoice_id BIGINT,
  amount DECIMAL(22,2),
  ts TIMESTAMP,
  primary key(id)); 

alter table line_item add constraint fk_line_item_invoice  foreign key (invoice_id) references invoice(id);
````

Note:

* The id columns are SERIAL, which means that they'll be auto-generated
* The ts column on line_item has a default value, so it can be omitted from inserts

### Row Objects

sqins expects case classes to represent rows (or entities if you prefer that term).  For example:

````scala
case class Invoice(id: Long = -1,
                   description: String,
                   image: Option[Array[Byte]] = None)

case class LineItem(id: Long = -1,
                    invoiceId: Long,
                    amount: BigDecimal,
                    ts: Timestamp = new Timestamp(System.currentTimeMillis())
````                    

Note:

* These classes have no references to sqins - they're plain case classes.
* Notice that the ids, which are autogenerated in the database, are given default values.  This allows us to omit them when constructing new rows.  Note - unlike some ORMs, sqins doesn't care about the value of this, so you can use whatever you want. 

### Tables

You define mappings from your row objects to the database using tables.

````scala
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
  val ts = Column[Timestamp]("ts")

  primaryKey(id)
  columns(id, invoice_id, amount, ts)
  
  // Set up a query directly inside our table
  def insert(row: LineItem) = INSERT INTO (this) VALUES (row) go
}
````

Note:

* Table takes two type parameters, the type of row and the type of the primary key column.
* Table takes one class parameter, which is the name of the table in the database
* Columns take one class parameter, namely the Scala type to which the column is mapped.  Type conversions are automatically brought in for columns using implicits.  If you attempt to map a column to a Scala type without an available type mapping, the compiler will complain.
* Auto-generated columns such as primary key columns can be flagged using the autoGenerated method.
* primaryKey(id) configures which column is the primary key.  Right now, sqins only supports single-column primary keys but we have multi-column key support on the roadmap.
* columns(id, description) tells sqins what order the relevant fields appear in the constructor of the Invoice case class.  The order must match, otherwise you will have problems.

#### Naming and aliasing
Since tables are defined as classes, in order to use them you need to instantiate them first.

````scala
val invoice = new InvoiceTable()
val line_item = new LineItemTable()
````

You can also alias tables, which has the same effect as in SQL.

````scala
val i = invoice AS "i"
val l = line_item AS "l"
````

### Setting up a Database object
Although it is not required, it is often useful to set up a Database object to make it easy to work with connections
and transactions, and also to collect all your table names and aliases in one place.

````scala
object db extends Database {
  // Set up our mechanism for getting connections
  Class.forName("org.postgresql.Driver");
  private val url = "jdbc:postgresql://localhost/sqins"
  private val props = new java.util.Properties()
  props.setProperty("user", "sqins")
  props.setProperty("password", "sqins")

  def openConnection() = DriverManager.getConnection(url, props);
  
  val invoice = new InvoiceTable()
  val i = invoice AS "i"
  val line_item = new LineItemTable()
  val li = line_item AS "li"
}
````

Note:

* sqins doesn't care where you get your connections, you just need to implement openConnection() to open a new
connection and give it to the Database.
* The trait Database provides methods withConnection and inTransaction which we'll see in use later.
* The Database class does transaction management on the database connection.  If you're using JTA or something like that,
don't use inTransaction().






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
    line_item.insert(LineItem(invoice_id = id, amount = i))
}

// Make the connection implicit so we don't have to keep using it explicitly
implicit val implicitConn = conn

// Update the amount on all of our line items
// Notice the use of ?() to bind the value 50
UPDATE (line_item) SET (line_item.amount := ?(50)) go

// You can also do a whole object update if you want
val updatedLineItem = LineItem(id = 1, invoice_id = 1, amount = 50)
UPDATE (line_item) SET (updatedLineItem) go

// Set up some aliases for our tables
val i = invoice AS "i"
val li = line_item AS "li"
val li2 = line_item AS "li2"

// You can use correlated subqueries in your update (also in INSERT and SELECT)
UPDATE (line_item) SET (line_item.amount := (SELECT (MAX(amount)) FROM li2 WHERE li2.id = line_item.id))

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

// You can also run arbitrary SQL like this
SQL("""put some really complicated sql in here""")(conn)
````

For an extended example, take a look at [core_tests.scala](sqins/blob/master/src/test/scala/org/sqins/core_tests.scala).

### Supported SQL Features

#### INSERT queries

````
INSERT INTO table [ ( column [, ...] ) ]
    VALUES ( expression [, ...] )
````

#### UPDATE queries

````
UPDATE table [ [ AS ] alias ]
    SET {{ column = expression } [,...] | row }
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

* Documentation
    * Create user guide
    * Include survey of other options (ScalaQuery, CircumflexORM, Squeryl)
    * Improve consistency of parentheses and explain their use
    * Clean up class and method visibility
    * Generate scaladoc
* All queries
    * Add these keywords: LIKE, ILIKE, IN, EXISTS
    * Provide nicer error messages by using @implicitNotFound
    * Provide operator similar to ScalaQuery ? to turn columns from an outer join into Option values
    * Support for multi-column primary keys
    * Support for no primary keys (already there, but needs testing)
    * Support for correlated subqueries in from clause
    * Indentation in query output for better readability
    * Support for type-casting functions???
    * Support for mapping to non-case classes???
* INSERT queries
    * Support for DEFAULT column values in the VALUES clause    
* SELECT queries
    * Support for UNION, INTERSECT and EXCEPT
* Database Support
    * MySQL
    * Oracle
    * SqlServer

### Setup in SBT

TODO: