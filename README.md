sqins - (s)tructured (q)ueries (in) (S)cala
-------------------------------------------

sqins provides type-safe SQL `INSERT`, `UPDATE`, `DELETE` and `SELECT` statements inside Scala using SQL-like syntax.
sqins makes 80% of typical DML really easy and stays out of the way for the other 20%.

**License Style** - [BSD 3 Clause](http://www.opensource.org/licenses/BSD-3-Clause)

**Status** - Alpha

### This is valid sqins

```scala
case class Invoice(id: Long = -1,
                   description: String,
                   image: Option[Array[Byte]] = None)

case class LineItem(id: Long = -1,
                    invoice_id: Long,
                    amount: BigDecimal,
                    ts: Timestamp = new Timestamp(System.currentTimeMillis())

// Assume that the entities Invoice and LineItem have been mapped by InvoiceTable and LineItemTable
// under db.i and db.li

db.inTransaction { implicit conn =>
  val insertedInvoiceId = (
    INSERT INTO db.i(db.i.description)
    VALUES (?("A new invoice"))
    RETURNING db.i.id)
    
  for (i <- 1 to 5) {
    val newLineItem = LineItem(invoice_id = insertedInvoiceId, amount = 5 * i)
    INSERT INTO db.li VALUES (newLineItem) go
  }
}

db.withConnection { implicit conn =>
  val query = (
    SELECT (db.i.*, db.li.*)
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    WHERE (db.i.description == ?("A new invoice")))
    
  query foreach { row =>
    println(row._1.id)
    println(row._1.description)
    println(row._2.id)
    println(row._2.invoice_id)
    println(row._2.amount)
    println(row._2.ts)
  }
}
```

## Benefits

 * Looks like SQL - if you know SQL, you pretty much know sqins.
 * Strongly typed - compile time checking for SQL syntax and column types.
 * Optimized - it's not quite as fast as using pure JDBC, but it's definitely low fat
 * Not an ORM - sqins is just a SQL API.
 * Simple mapping - unlike an ORM, sqins just needs to know the most basic things about your tables and columns.
 * Extensible scalar types - sqins comes with support for basic types like numbers, strings and dates and makes it super-easy to define new type mappings.
 * Side-effect free - sqins doesn't cache data, maintain identity or relationships or do any other ORM funkiness.  This makes it easier to use, and makes it usable in idiomatic Scala.
 * Data objects don't depend on sqins - unlike with some ORMs, data objects have no reference to any sqins artifacts, so you can freely pass them around the network. 
 * Smart performance - sqins employs common-sense optimizations like using PreparedStatements, streaming results from cursors, etc.
 * No SQL-injection - by using PreparedStatements and bound parameters, sqins keeps you out of SQL injection trouble.
 * Works with PostgreSQL - other databases are on the way.
 * Runs in your container - sqins doesn't care where you get your connections, so it works equally well in containerless and in-container apps.
 * Friendly error messages - if something goes wrong while executing SQL, sqins gives you the information you need to debug it
 * See your SQL - just use the `toString` method on a query object to see the underlying SQL string
 
## Other Good Options

* [scalaquery](http://scalaquery.org/) - Seems to be one of the more popular ones
* [squeryl](http://squeryl.org/) - Also fairly popular
* [CircumflexORM](http://circumflex.ru/projects/orm/index.html) - Syntax served as inspiration for sqins, but it's an ORM  

## Installation (with SBT)

sqins is hosted on Sonatype's OSS repository.  Make sure that you have the necessary resolvers:

```scala
resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype OSS releases" at "http://oss.sonatype.org/content/repositories/releases"
)
```

Add a dependency for sqins.  The latest version is 0.1-SNAPSHOT

```scala
libraryDependencies += "org.sqins" % "sqins_2.9.1" % "0.1-SNAPSHOT"
```

## Imports

You'll typically use these 2 imports.

```scala
import org.sqins._
import org.sqins.Implicits._
```

## Mapping

### Example Database Schema

This is SQL, not Scala.

```sql
DROP TABLE IF EXISTS line_item;
DROP TABLE IF EXISTS invoice;

create table invoice (
  id SERIAL,
  description VARCHAR(255) NOT NULL,
  image BYTEA,
  primary key(id));

create table line_item (
  id SERIAL,
  invoice_id BIGINT NOT NULL,
  amount DECIMAL(22,2) NOT NULL,
  ts TIMESTAMP,
  primary key(id)); 

alter table line_item add constraint fk_line_item_invoice
  foreign key (invoice_id) references invoice(id);
```

* The `id` columns are `SERIAL`, which means that they'll be auto-generated
* The `ts` column on `line_item` has a default value, so it can be omitted from inserts

### Row Objects

sqins expects classes (regular or case) to represent rows (or entities if you prefer that term).  For example:

```scala
case class Invoice(id: Long = -1,
                   description: String,
                   image: Option[Array[Byte]] = None)

class LineItem(val id: Long = -1,
               val invoiceId: Long,
               val amount: BigDecimal,
               val ts: Timestamp = new Timestamp(System.currentTimeMillis())
```                    

* These classes have no references to sqins - they're plain classes.
* Notice that the ids, which are autogenerated in the database, are given default values.  This allows us to omit them when constructing new rows.  Unlike ORMs, sqins doesn't care about the value of this, so you can use whatever you want.
* Notice that we're not mapping a relationship from `LineItem` to `Invoice`, just the foreign key itself.  sqins doesn't map associations, but as you'll see later `SELECT` queries allow powerful joins that make this bit of ORM complexity unnecessary. 

### Tables

You define mappings from your row objects to the database by extending the class `Table[T, K]`.

```scala
class InvoiceTable extends Table[Invoice]("invoice") {
  val id = Column[Long]("id").autoGenerated
  val description = Column[String]("description")

  columns(id, description)
}

class LineItemTable extends Table[LineItem]("line_item") {
  val id = Column[Long]("id").autoGenerated
  val invoice_id = Column[Long]("invoice_id")
  val amount = Column[BigDecimal]("amount")
  val ts = Column[Timestamp]("ts")

  columns(id, invoice_id, amount, ts)
  
  // Set up a query directly inside our table
  def insert(row: LineItem) = INSERT INTO (this) VALUES (row) RETURNING row.id go
}
```

* `Table` takes one type parameter, the type of row.
* `Table` takes one class parameter, which is the name of the table in the database.
* `Column` takes one parameter, namely the Scala type to which the column is mapped.  Type mappings are automatically brought in for columns using implicits.  If you attempt to map a `Column` to a Scala type without an available type mapping, the compiler will complain.
* Columns are assigned to vals so that they can be referenced in query expressions (you'll see this used later).
* Nullable columns like `image` are mapped with an Option type.
* Auto-generated columns such as primary key columns can be flagged using the `autoGenerated` method.
* `columns(id, description)` tells sqins what order the relevant fields appear in the constructor of the Invoice class.  The order must match, otherwise you will have problems.
* Tables are one of several good locations to collect your actual queries.  sqins doesn't care whether you put your queries in a single place or scatter them throughout your code.
* Building queries doesn't require a database connection and they are immutable, so you can build them anywhere at any time.
* Running queries of course requires a database connection, which by its definition involves side effects and dependence on an unreliable and complex resource.  So, you should think carefully about what parts of your codebase actually execute queries.  Ideally, this will be isolated so that most of the system can still be operated and verified without a database. 

#### Naming and aliasing
Since tables are defined as classes, in order to use them you need to instantiate them first.

```scala
val invoice = new InvoiceTable()
val line_item = new LineItemTable()
```

You can also alias tables, which has the same effect as in SQL.  This becomes important when doing complex queries in which the same table may appear in multiple roles.

```scala
val i = invoice AS "i"
val l = line_item AS "l"
```

### Type Mappings

sqins maps fields to database columns using `TypeMapping` objects, which convert between the Scala type and the appropriate JDBC type.

sqins provides a `TypeMapping` for each the common basic types, and you can easily add your own.

Note - a type and its option type are considered different types, and have their own type mappings.  For example, `String` and `Option[String]` have different TypeMappings.

#### How TypeMappings are used
sqins uses `TypeMapping` implicitly.  All constructors and methods that require a `TypeMapping` (for example `Column`) accept implicit TypeMappings.  By importing `org.sqins.Implicits._` you import all of the built-in implicit TypeMappings, so you usually don't need to reference them explicitly.

#### Included TypeMappings

sqins includes TypeMappings for the following Scala types (and their related Option types):

`Byte`  
`Short`  
`Int`  
`Long`  
`Float`  
`Double`  
`Boolean`  
`String`  
`scala.math.BigDecimal`  
`java.sql.Date`  
`java.sql.Timestamp`  
`java.util.UUID` (mapped to String)
`java.net.URL`   (mapped to String)  
`Array[Byte]`  

#### Defining Custom TypeMappings

Since the usual style is to use `TypeMapping` implicitly, you should define your own TypeMappings as implicits as well.

A mapping for a regular type extends the `TypeMapping[T]` trait and implements the `_get` and `_set` methods.

Here's how we can define a mapping for a new type `MyType`:

```scala
case class MyType(wrapped: String)

object MyTypeMappings {
  implicit object MyTypeMapping extends TypeMapping[MyType] {
    def _get(rs: java.sql.ResultSet, position: Int) =
      Extraction(MyType(rs.getString(position)), 1)
    
    def _set(ps: java.sql.PreparedStatement, position: Int, value: MyType) =
      ps.setString(position, value.wrapped)
  }

  // Option types can usually be mapped by just wrapping
  // the regular TypeMapping with an OptionTypeMapping
  implicit val OptionMyTypeMapping = new OptionTypeMapping(MyTypeMapping)
}

// To use our new type mappings, just import them
import MyTypeMappings._
```

* `Extraction` is an object that includes both the extracted value and the number of columns that were used to extract it.  Right now, sqins has only been tested with single-column values but we may eventually add support for composites.

## Setting up a Database object
Although it is not required, it is often useful to set up a Database object to make it easy to work with connections
and transactions, and also to collect all your table names and aliases in one place.

```scala
object db extends Database {
  // Our example's hoaky mechanism for getting connections
  Class.forName("org.postgresql.Driver");
  private val url = "jdbc:postgresql://localhost/sqins"
  private val props = new java.util.Properties()
  props.setProperty("user", "sqins")
  props.setProperty("password", "sqins")

  def openConnection() = DriverManager.getConnection(url, props);
  
  val invoice = new InvoiceTable()
  val i = invoice AS "i"
  val i2 = invoice AS "i2"
  val i3 = invoice AS "i3"
  val line_item = new LineItemTable()
  val li = line_item AS "li"
  val li2 = line_item AS "li2"
  val li3 = line_item AS "li3"
}
```

* sqins doesn't care where you get your connections, you just need to implement `openConnection()` to open a new
connection and give it to the Database.  For server applications, it's always a good idea to use a connection pool.
* The trait `Database` provides methods `withConnection` and `inTransaction` which we'll see in use later.
* The `Database` class does transaction management using the database connection.  If you're using JTA or something like
that, don't use `inTransaction()`.
* It's a good idea to go ahead and define several aliases for the same table in case you need them.  Since `db` is a
singleton object, it doesn't cost much and it's very convenient.

## Querying

This section is meant to be read in order--each sub-section builds on the next.  You can find the complete
[grammar](#grammar) at the end of this section. 

### SELECT Queries
SELECT queries in sqins are functions that take an implicit `java.sql.Connection` and return a `SelectResult[T]` representing
the resulting rows.  `SelectResult[T]` is an `Iterable[T]` backed by the ResultSet from the database.  Type type of the
each result is based on what appears in the SELECT clause.

Our examples are based on the [mapping shown above](#tables).

#### SELECT basics

```scala
val query = SELECT (db.invoice.id) FROM (db.invoice)

db.withConnection { conn => 
  val result:Iterable[Long] = query(conn);
  
  // You can only iterate over result while the Connection is still open
  result.foreach { row =>
    println(row * 5)
  }
  
  // To return everything, you could have done this:
  result.toList
  
  // To get just the first row, use the firstOption method
  val first: Option[Long] = result.firstOption
  
  // The resulting list doesn't depend on the database Connection
}
```

Since id is a Column of type `Long`, the results are also of type `Long`.

Note the parentheses around the parameters in the SELECT clause.  This is a little different than SQL, but we have to
live with it in sqins.  For single-table queries, you can leave off the parentheses around the table at least.

```scala
val query = SELECT (db.invoice.id) FROM db.invoice
```

Queries also support the method `go`.  Since they accept implicit connections, you can use this syntax as well.

```scala
val query = SELECT (db.invoice.id) FROM db.invoice

db.withConnection { implicit conn => 
  val result:Iterable[Long] = query go
}   
```

In fact, as long as there's an implicit Connection in scope, SELECT queries can be directly treated as their result
without having to call the `go` method.

```scala
val query = SELECT (db.invoice.id) FROM db.invoice

db.withConnection { implicit conn => 
  query.foreach { row =>
    println(row * 5)
  }
}   
```

Queries can return multiple values, in which case the result will be a Tuple with arity matching the number of elements
in the SELECT clause.

```scala
val query2 = SELECT (db.invoice.id, db.invoice.description, db.invoice.image) FROM db.invoice

db.withConnection { implicit conn => 
  query2.foreach { row: Tuple3[Long, String, Option[Array[Byte]]] =>
    println(row._1 * 5)
    println(row._2 + " more string")
    println(row._3)
  }
}   
```

This is all well and good, but often we may want to read our actual row objects.  For this, sqins provides the * operator.

```scala
val query3 = SELECT (db.invoice.*) FROM db.invoice

db.withConnection { implicit conn => 
  query3.foreach { row: Invoice =>
    println(row.id * 5)
    println(row.description + " more string")
    println(row.image)
  }
} 
```

#### Joins

We can also join across tables, using INNER_JOIN, LEFT_OUTER_JOIN and RIGHT_OUTER_JOIN.

```scala
val query4 = (
  SELECT (db.i.*, db.li.*)
  FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id))
  
db.withConnection { implicit conn => 
  query4.foreach { row: Tuple2[Invoice, LineItem] =>
    println(row._1.id * 5)
    println(row._1.description + " more string")
    println(row._1.image)
    println(row._2.id * 4)
    println(row._2.invoice_id)
    println(row._2.amount)
    println(row._2.ts)
  }
}   
```

When using outer joins, some values may actually come back null.  We use the .? operator to turn such select arguments
into Option values.
```scala
val query4_1: SelectQuery[Tuple2[Invoice, Option[LineItem]]] = (
  SELECT (db.i.*, db.li.*.?)
  FROM (db.i LEFT_OUTER_JOIN db.li ON db.i.id == db.li.invoice_id)))
```

The .? operator also works on individual columns:
```scala
val query4_1: SelectQuery[Tuple2[Invoice, Option[Long]]] = (
  SELECT (db.i.*, db.li.id.?)
  FROM (db.i LEFT_OUTER_JOIN db.li ON db.i.id == db.li.invoice_id)))
```

We're now enclosing the entire query expression in parentheses to support breaking it onto multiple lines.

Note how we use the `==` operator instead of `=`.  Other comparison operators are the same as in SQL, namely
`<>`, `<`, `<=`, `>`, `>=`, `LIKE` and `ILIKE`.

Of course, we can also select individual columns even when joining.

```scala
val query5 = (
  SELECT (db.i.id, db.li.amount)
  FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id))
  
db.withConnection { implicit conn => 
  query5.foreach { row: Tuple2[Long, BigDecimal] =>
    println(row._1)
    println(row._2)
  }
}   
```

#### WHERE clause

Query results can be restricted using a WHERE clause.  The syntax for conditions inside the WHERE clause is the same as
in the INNER_JOIN ON clause.

```scala
SELECT (db.i.id, db.li.amount)
FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
WHERE (db.li.amount > db.li.id)
```

Conditions are composed using && and || in place of AND and OR.

```scala
SELECT (db.i.id, db.li.amount)
FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id && db.li.id <> db.i.id)
WHERE (db.li.amount > db.li.id || db.li.amount == db.li.amount)
```

The logical negation operator NOT can be applied to any condition.

```scala
SELECT (db.i.id, db.li.amount)
FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
WHERE (db.li.amount > db.li.id && NOT(db.li.amount <> db.li.amount))
```

The IN and EXISTS operators are supported as well.
```
SELECT (db.i.*)
FROM db.i
WHERE (db.i.id IN ?(Seq(1, 2, 3)))
```

IN can also take a SELECT query.
```
SELECT (db.i.*)
FROM (db.i)
WHERE (db.i.id IN (SELECT (db.i2.id)
  FROM (db.i2)
  WHERE db.i.id == db.i2.id))
```

EXISTS requires a SELECT query.
```
SELECT (db.i.*)
FROM (db.i)
WHERE EXISTS (SELECT (db.i2.id)
  FROM (db.i2)
  WHERE db.i.id == db.i2.id)
```

#### Aggregate Queries

sqins supports GROUP_BY clauses and aggregate functions.

```scala
val query6:SelectQuery[Tuple2[Long, BigDecimal]] = (
  SELECT (db.i.id, MAX(db.li.amount))
  FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
  GROUP_BY (db.i.id))
```

The following aggregate functions are supported out of the box:

`AVG`  
`COUNT`  
`COUNT_DISTINCT`  
`COUNT_*`  
`MIN`  
`MAX`  
`SUM`  
`VAR_POP`  
`VAR_SAMP`  

#### Bound Values

Queries can be parameterized using bound values.  Bound values bind Scala values into your query.

Bound values are implemented using positional parameter binding in `PreparedStatements` in order to avoid SQL injection
and allow the PreparedStatements to be cached (if your datasource does that).

```scala
val invoiceId = 5

val query7 = (
  SELECT (db.i.id, db.li.amount)
  FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
  WHERE (db.i.id == ?(invoiceId)))
```

Of course one can bind any valid Scala expression, constant or otherwise.

```scala
SELECT (db.i.id, db.li.amount)
FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
WHERE (db.i.id == ?(5) || db.i.id == ?(5 * 35 + 3))
```

## Plug In Arbitrary SQL with EXPR and VEXPR

sqins doesn't support the entire syntax of every database, but it often comes close.  For those times when your need a
little more than native sqins can give, use EXPR and VEXPR to plug in SQL with a string.

EXPR takes any String and allows you to use it as a scalar expression and even as a condition.

```scala
SELECT (db.i.id, db.li.amount)
FROM (db.i INNER_JOIN db.li ON EXPR("db.i.id funky_database_operator db.li.invoice_id"))
```

VEXPR is similar to EXPR except that it's typed and can be used as a scalar value.

```scala
SELECT (db.i.id, VEXPR[BigDecimal]("db.li.amount * 5"))
FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
```

As you'll see later, you can actually write queries that are pure strings.  Of course, the more strings you use,
the less type safety you get.  EXPR and VEXPR are useful because they allow you to keep as much of your query type-safe
as possible, while allowing you to take advantage of special database features where you need.

WARNING - using EXPR, VEXPR and pure SQL queries introduces the possibility of SQL-injection.  Be careful! 

#### Functions

You can use scalar functions that take one or more parameters using FN("name of function").

For example, to use the database function "lower" which converts a String argument to lowercase:

```scala
SELECT (db.i.id)
FROM db.i
WHERE FN("lower")(db.i.description) == ?("my lowercase description") 
```

The function's return type is always the same as the specified value.

Aggregate expressions are just functions, so if you need to do a special aggregate that's not built-into SQINS,
you can use FN.

```scala
val query8:SelectQuery[Tuple2[Long, BigDecimal]] = (
  SELECT (db.i.id, FN("SPECIAL_AGGREGATE")(db.li.amount))
  FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
  GROUP_BY (db.i.id))
```

Even better, add your functions to your custom implicits for easy reuse.

```scala
object MyImplicits {
  implicit def SPECIAL_AGGREGATE = FN("SPECIAL_AGGREGATE") 
}

import MyImplicits._

val query9:SelectQuery[Tuple2[Long, BigDecimal]] = (
  SELECT (db.i.id, SPECIAL_AGGREGATE(db.li.amount))
  FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
  GROUP_BY (db.i.id))
```

### INSERT Queries

INSERT queries allow you to insert either specific values:

```scala
db.withConnection { implicit conn =>
  val rowsInserted: Int = INSERT INTO db.invoice(db.invoice.description) VALUES (?("My Description"))
}
```

or whole rows
```scala
val newInvoice = Invoice(description = "My Description")

db.withConnection { implicit conn =>
  val rowsInserted: Int = INSERT INTO db.invoice VALUES(newInvoice)
}
```

Either way, they return the number of rows inserted.

Often, you may want to return the primary key or the whole inserted row.  INSERT supports the RETURNING clause, which
causes the query to return whatever was specified in the RETURNING clause.
```scala
db.withConnection { implicit conn =>
  val insertedId: Long = (
    INSERT INTO db.invoice(db.invoice.description)
    VALUES (?("My Description"))
    RETURNING db.invoice.id)
  
  val insertedInvoice: Invoice = (
    INSERT INTO db.invoice(db.invoice.description)
    VALUES (?("My Description"))
    RETURNING db.invoice.*)
}
```

Some databases don't support the RETURNING clause.  For these, it is still possible to get at the ids using Sqins'
RETURNING_IDS clause.  Unlike RETURNING, this can only return auto-generated ids, not whole records.
```scala
db.withConnection { implicit conn =>
  val insertedId: Long = (
    INSERT INTO db.invoice(db.invoice.description)
    VALUES (?("My Description"))
    RETURNING_IDS db.invoice.id)
}
```

INSERT queries also allow you to insert using a SELECT statement.

```scala
db.withConnection { implicit conn =>
  val numberOfInsertedRows: Int = (
    INSERT INTO db.invoice(db.invoice.description)
    SELECT (db.invoice.description) FROM db.invoice)
}
```

INSERT ... SELECT ... also supports a RETURNING clause.  In this case, it returns a SelectResult representing all
inserted rows.
```scala
db.withConnection { implicit conn =>
  val insertedIds: SelectResult[Long] = (
    INSERT INTO db.invoice(db.invoice.description)
    SELECT (db.invoice.description) FROM db.invoice
    RETURNING db.invoice.id)
}
```

### UPDATE Queries

UPDATE queries allow you to update individual columns of a table:

```scala
db.withConnection { implicit conn =>
  val numberOfUpdatedRows: Int = (
    UPDATE (db.invoice)
    SET (db.invoice.description := ?("New description")))
}
```

or entire rows

```scala
val updatedInvoice = Invoice(id=5, description="New description")

db.withConnection { implicit conn =>
  val numberOfUpdatedRows = (
    UPDATE (db.invoice)
    SET (updatedInvoice))
}
```

Notice the use of `:=` in place of the usual SQL `=`.

You can also use basic expressions like the concatenation operator to update values in place:

```scala
UPDATE (db.invoice)
SET (db.invoice.description := db.invoice.description || ?(" with additional text"))
```

UPDATE queries support a WHERE clause just like SELECT queries:

```scala
db.withConnection { implicit conn =>
  val numberOfUpdatedRows: Int = (
    UPDATE (db.invoice)
    SET (db.invoice.description := ?("New description"))
    WHERE (db.invoice.id <= ?(5)))
}
```

UPDATE queries also support a RETURNING clause, just like INSERT queries:
```scala
db.withConnection { implicit conn =>
  val updatedRows: SelectResult[Invoice] = (
    UPDATE (db.invoice)
    SET (db.invoice.description := ?("New description"))
    WHERE (db.invoice.id <= ?(5))
    RETURNING db.invoice.*)
}
```

### DELETE Queries

DELETE queries work as one would expect:

```scala
db.withConnection { implicit conn =>
  val numberOfUpdatedRows: Int = (
    DELETE FROM db.invoice
    WHERE (db.invoice.id <= ?(5)))
}
```

DELETE queries also support a RETURNING clause, just like UPDATE queries:
```scala
db.withConnection { implicit conn =>
  val updatedIds: SelectResult[Long] = (
    DELETE FROM db.invoice
    WHERE (db.invoice.id <= ?(5))
    RETURNING db.invoice.id)
}
```

### Sub-Queries

SELECT queries can appear as correlated sub-queries inside of a SELECT clause, the SET clause of an UPDATE query and
the WHERE clause of both SELECT and UPDATE queries. 

```scala
SELECT (db.i.id, (SELECT (MAX(db.li.id)) FROM db.li WHERE db.li.invoice_id == db.i.id))
FROM (db.i)
WHERE (db.i.id == (SELECT (MAX(db.li.invoice_id)) FROM db.li))
```

### How I learned to stop worrying about sqins syntax and love the view

Sometimes, you really need to do some super-awesome SQL trickery and sqins' syntax just doesn't cut it.  May I
suggest writing a view and mapping from sqins to the view?  This lets you use your database for what it's good at, and
it allows you to reuse that awesome SQL from other tools.

### Pure SQL Queries

Don't like views?  Need to do some weird INSERT or UPDATE queries?  Want to run DDL?

When sqins just won't do, you can also do pure SQL queries including bind parameters:

```scala
db.withConnection { implicit conn =>
  val ps: PreparedStatement = SQL("DELETE FROM invoice WHERE id <= ?", ?(5)).executeUpdate
  val rowsInserted: Int = ps.getUpdateCount()
}
```

There's also an `executeQuery` method that returns a ResultSet like you would expect.
 
```scala
db.withConnection { implicit conn =>
  val ps: PreparedStatement = SQL("SELECT * FROM invoice").executeQuery
  val rs: java.sql.ResultSet = ps.getResultSet()
}
```

### Grammar

This section describes the full grammar of sqins.

A `SELECT` query is:
```
SELECT [DISTINCT] (extractable_expression)
  FROM (from_item)
  [WHERE (condition)]
  [ORDER_BY (expression)]
  [GROUP_BY (expression)]
  [LIMIT bound_value]      <- database-specific
  [OFFSET bound_value]     <- database-specific
```

An `INSERT` query is:
```
INSERT INTO table [(column [, ...])]
  { VALUES ({ bound_value [, ...] | row_object }) |  a SELECT query }
  [RETURNING extractable_expression | RETURNING_IDS extractable_expression]
```

An `UPDATE` query is:
```
UPDATE (table)
  SET ({ expression | row_object })
  [WHERE (condition)]
  [RETURNING extractable_expression]
```

A `DELETE` query is
```
DELETE FROM table
  [WHERE (condition)]
  [RETURNING extractable_expression]
```

A pure `SQL` query is
```
SQL("a sql string")
```

*expression* is:
```
{ scalar_expression | set_expression } [, ...]
```

*scalar_expression* is:
```
{ scalar_value [ ASC | DESC ] | EXPR("custom SQL") }
```

*set_expression* is:
```
scalar_expression := scalar_value
```

*extractable_expression* is:
```
extractable_scalar [, ...]
```

*extractable_scalar* is:
```
{ scalar_value | another SELECT query }
```

*scalar_value* is:
```
{ column | projection | scalar_function_call | bound_value |
  VEXPR("custom SQL") | scalar_value operator scalar_expression }
```

*operator* is:
```
{ + | - | * | / | || }
```

*scalar_function_call* is:
```
{ predefined_function | FN("function name") } ({ scalar_value | expression })
```

*projection* is:
```
table.*
```

*bound_value* is:
```
?(any value from your code, like a variable or a constant expression)
```

*row_object* is:
```
An instance of a class representing a row from the table being inserted/updated
```

*from_item* is:
```
table [{ INNER_JOIN | LEFT_OUTER_JOIN | RIGHT_OUTER_JOIN } table ON condition ...]
```

*condition* is:
```
{ unary_condition | binary_condition | in_condition | exists_condition | EXPR("custom SQL") } [{ && | || } condition]
```

Any condition can also be negated by using NOT
```
NOT(condition)
```

*unary_condition* is:
```
scalar_expression { IS_NULL | IS_NOT_NULL }
```

*binary_condition* is:
```
scalar_expression { == | <> | != | > | >= | < | <= | LIKE | ILIKE } scalar_expression }
```

*in_condition* is:
```
scalar_value IN { sequence_of_bound_value | select_query }
```

*exists_condition* is:
```
EXISTS select_query
``` 

## Roadmap

### Release 0.1

* All queries
    * Indentation in query output for better readability
* SELECT queries
    * Support for UNION, INTERSECT and EXCEPT
* Database support
    * PostgreSQL
    
### Release 0.2

* Database Support
    * h2

* All queries
    * Support for non-case classes
    * Support for RETURNING_IDS clause (for databases that can't do RETURNING)
    
### Future Possibilities

* All queries
    * Support for mapping composite types (multiple columns per value)
    * Support for type-casting functions
    * Support for correlated subqueries in from clause
* INSERT queries
    * Support for DEFAULT column values in the VALUES clause    
* UPDATE queries
    * Tighten up type checking for the SET expression (right now, it's just any expression)