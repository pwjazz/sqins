sqins (structured queries in Scala)
===================================

sqins makes SQL available inside Scala.  sqins is not an ORM, just a SQL API.  sqins makes the 80% of typical DML really
easy and stays out of the way for the other 20%.

Key Features:

- Strongly typed - compile time checking for SQL syntax and column types
- Syntax very similar to SQL - if you know SQL, you pretty much know sqins
- Simple mapping - unlike an ORM, sqins doesn't need to know much about your tables
- Stateless - no caching or other ORM weirdness
- Smart performance - sqnis employs common-sense optimizations like using PreparedStatements, streaming results from cursors, etc.

== Setup in SBT

TODO:

== Mapping your tables

// Rows are modeled as case classes (no dependencies on sqins API here)
case class Invoice(id: Long, description: String)

// This is a table whose rows are typed as Invoice and which is named "invoice" in the database
object Invoice extends Table[Invoice]("invoice") {
  // Columns are fields of the Table, strongly typed and given the name as used in the database
  val id = Column[Long]("id")
  val description = Column[String]("description")

  // The columns variable needs to be set a sequence of all the table's columns, in the same order as they appear in
  // the row case class constructor
  columns = Seq(id, description)
}

// Build a basic select query (doesn't require a database connection)
val query = SELECT (Invoice.*) FROM (Invoice)

// sqins queries use regular java.sql.Connections.  Get them wherever you like (datasource, etc.)
val conn:java.sql.Connection = ...

// The query is a function that takes a connection and returns an Iterable of Invoices
val result:Iterable[Invoice] = query(conn)

// Rows are streamed from the ResultSet, not read into memory as a List
result.forEach { row => ... }

// If you need to close your connection and return all rows, just use the toList method on Iterable
val resultList = result.toList

// You can also use aliases
val i = Invoice as "i"

val queryWithAlias = SELECT (i.*) FROM (i)

// You can break queries onto multiple lines by wrapping the query in parentheses
val multiLineQuery = (
  SELECT (i.*)
  FROM (i))
  
// You can select individual columns
val tupleQuery = SELECT (i.id, i.description) FROM (i)

// This gives your results as tuples
val tupleResult:Iterable[Tuple2(Long, String)] = tupleQuery(conn)

// You can even mix and match rows and tuples
val rowAndTupleQuery = SELECT (i.*, i.description) FROM (i)
var rowAndTupleResult:Iterable[Tuple2(Invoice, String)] = rowAndTupleQuery(conn)

