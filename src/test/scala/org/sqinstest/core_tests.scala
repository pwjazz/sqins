/*
Copyright (c) 2012, Percy Wegmann
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/

package org.sqinstest

import java.sql.{ Connection, DriverManager, Timestamp, PreparedStatement, ResultSet }
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.sqins._
import org.sqins.Implicits._

/**
 * Test the basic operation of sqins queries
 */
class CoreSpec extends FlatSpec with ShouldMatchers {
  // Set up variables for tables
  val invoice = new InvoiceTable()
  val line_item = new LineItemTable()

  // Set up aliases too
  val i = invoice AS "i"
  val li = line_item AS "li"

  "Schema" should "be initializable" in {
    db.withConnection { implicit conn =>
      db.initSchema.executeUpdate
    }
  }

  "A SQL query" should "provide useful error reporting" in {
    // ?(5) binds the value 5 into the query
    val query = SQL("INVALID_SQL ?", ?(5))

    try {
      db.withConnection { implicit conn =>
        query.executeQuery(conn)
      }
    } catch {
      case e: Exception => {
        e.getClass() should equal(classOf[SQLError])
        e.getMessage.indexOf("""Error executing SQL "INVALID_SQL ?" with params (5) failed: ERROR: syntax error at or near "INVALID_SQL"""") should equal(0)
      }
    }
  }

  var insertedInvoiceId: Long = null.asInstanceOf[Long]

  "The INSERT function" should "be able to construct an insert query using full table names and specific columns, and return any and all columns from the table" in {
    // Define some query builder methods
    def insertInvoice(invoice: Invoice) = (
      INSERT INTO i(i.description)
      VALUES (?(invoice.description))
      RETURNING (i.id))

    val invoice = Invoice(description = "An invoice")
    db.withConnection { implicit conn =>
      insertedInvoiceId = insertInvoice(invoice) go
    }
    insertedInvoiceId should equal(1)
  }

  it should "also be able to construct an insert query using table aliases and whole rows" in {
    val ia = i AS "ia"
    val lia = li AS "lia"

    def insertLineItem(lineItem: LineItem) = (
      INSERT INTO li
      VALUES (lineItem))

    val lineItem = LineItem(invoice_id = insertedInvoiceId, amount = 25)
    db.withConnection { implicit conn =>
      insertLineItem(lineItem) go
    }
  }
  
  it should "also allow INSERT ... SELECT FROM semantics" in {
    val query = (INSERT INTO invoice(invoice.description, invoice.image)
      SELECT (invoice.description, invoice.image) FROM invoice WHERE invoice.id == ?(-1))

    query.insertExpression should equal("""|INSERT INTO invoice (description, image)
                                          |SELECT invoice.description, invoice.image
                                          |FROM invoice
                                          |WHERE invoice.id = ?""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn) should equal(0)
    }
  }

  it should "also allow INSERT ... SELECT FROM ... RETURNING" in {
    val query = (INSERT INTO invoice(invoice.description, invoice.image)
      SELECT (invoice.description, invoice.image)
      FROM invoice
      WHERE invoice.id == ?(-1)
      RETURNING invoice.*)

    query.insertExpression should equal("""|INSERT INTO invoice (description, image)
                                          |SELECT invoice.description, invoice.image
                                          |FROM invoice
                                          |WHERE invoice.id = ?
                                          |RETURNING invoice.id, invoice.description, invoice.image""".stripMargin)

    db.withConnection { implicit conn =>
      query.toList.length should equal(0)
    }
  }

  it should "also allow INSERT ... SELECT FROM semantics using a predefined sub query" in {
    val subQuery = SELECT(invoice.description, invoice.image) FROM invoice WHERE invoice.id == ?(-1)
    val query = (INSERT INTO invoice(invoice.description, invoice.image))(subQuery)

    query.insertExpression should equal("""|INSERT INTO invoice (description, image)
                                          |SELECT invoice.description, invoice.image
                                          |FROM invoice
                                          |WHERE invoice.id = ?""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn) should equal(0)
    }
  }

  "An UPDATE query" should "return the number of rows updated" in {
    val query = (
      UPDATE(li)
      SET (li.amount := li.amount + ?(5), li.invoice_id := li.invoice_id))

    db.withConnection { implicit conn =>
      query(conn) should equal(1)
    }
  }

  it should "support a WHERE clause" in {
    val query = (
      UPDATE(li)
      SET (li.amount := ?(56.79))
      WHERE li.invoice_id == ?(1) && li.invoice_id <> li.id)

    db.withConnection { implicit conn =>
      query(conn) should equal(0)
    }
  }

  it should "allow setting whole rows" in {
    val newLineItem = LineItem(id = 1, invoice_id = 1, amount = 56.78)

    val query = UPDATE(li) SET (newLineItem) WHERE (li.id == ?(newLineItem.id))

    query.updateExpression should equal("""|UPDATE line_item AS li
                                     |SET invoice_id = ?, amount = ?
                                     |WHERE li.id = ?""".stripMargin)
    db.withConnection { implicit conn =>
      query(conn) should equal(1)
    }
  }

  it should "support a RETURNING clause" in {
    val newLineItem = LineItem(id = 1, invoice_id = 1, amount = 56.78)

    val query = (
      UPDATE(li)
      SET (newLineItem)
      WHERE li.id == ?(newLineItem.id)
      RETURNING li.*)

    query.updateExpression should equal("""|UPDATE line_item AS li
                                     |SET invoice_id = ?, amount = ?
                                     |WHERE li.id = ?
                                     |RETURNING li.id, li.invoice_id, li.amount, li.ts""".stripMargin)

    db.withConnection { implicit conn =>
      query.foreach { updated =>
        updated.amount should equal(56.78)
      }
    }
  }

  it should "allow setting values to correlated subqueries" in {
    val newLineItem = LineItem(id = 1, invoice_id = 1, amount = 56.78)

    val li2 = line_item AS "li2"

    val query = UPDATE(li) SET (li.amount := (
      SELECT(MAX(li2.amount))
      FROM li2
      WHERE li2.id == li.id && li2.id <> ?(-1)))

    query.updateExpression should equal("""|UPDATE line_item AS li
                                           |SET amount = (SELECT MAX(li2.amount)
                                           |FROM line_item AS li2
                                           |WHERE li2.id = li.id AND li2.id <> ?)""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn) should equal(1)
    }
  }

  var simpleSelectQuery: SelectQuery[Invoice] = null
  "A simple SELECT statement" should "be constructable using projections" in {
    simpleSelectQuery = SELECT(i.*) FROM i
  }

  var simpleSelectResult: SelectResult[Invoice] = null
  it should "act as a function that takes a connection and returns a typed SelectResult" in {
    db.withConnection { implicit conn =>
      simpleSelectResult = simpleSelectQuery(conn)
    }
  }

  "A SelectResult" should "include the correct number of rows" in {
    simpleSelectResult.toList.length should equal(1)
  }

  it should "allow iteration over the rows" in {
    simpleSelectResult.foreach(row => {
      row.id should equal(1)
      row.description should equal("An invoice")
    })
  }

  "A SELECT statement" should "be able to return scalar values" in {
    val query: SelectQuery[Long] = SELECT(i.id) FROM i

    query.queryExpression should equal(
      """|SELECT i.id
           |FROM invoice AS i""".stripMargin)
  }

  it should "be writeable on multiple lines by wrapping in parentheses" in {
    val query: SelectQuery[Long] = (
      SELECT(i.id)
      FROM i)

    query.queryExpression should equal(
      """|SELECT i.id
           |FROM invoice AS i""".stripMargin)
  }

  it should "be able to contain bound scalar values in the select clause" in {
    val query: SelectQuery[Int] = SELECT(?(5)) FROM i

    query.queryExpression should equal(
      """|SELECT ?
           |FROM invoice AS i""".stripMargin)
  }

  it should "be able to select individual fields into tuples" in {
    val query: SelectQuery[Tuple2[Long, String]] = SELECT(i.id, i.description) FROM i
  }

  it should "be able to contain INNER joins" in {
    val query: SelectQuery[Tuple2[Invoice, LineItem]] = (
      SELECT(i.*, li.*)
      FROM (i INNER_JOIN li ON i.id == li.invoice_id))

    query.queryExpression should equal(
      """|SELECT i.id, i.description, i.image, li.id, li.invoice_id, li.amount, li.ts
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).foreach(row => {
        row._1.id should equal(1)
        row._1.description should equal("An invoice")
        row._2.id should equal(1)
        row._2.invoice_id should equal(1)
        row._2.amount should equal(56.78)
      })
    }
  }
  
  it should "be able to contain LEFT OUTER joins" in {
    val query: SelectQuery[Tuple2[Invoice, Option[Long]]] = (
      SELECT(i.*, li.id.?)
      FROM (i LEFT_OUTER_JOIN li ON i.id == li.invoice_id))

    query.queryExpression should equal(
      """|SELECT i.id, i.description, i.image, li.id
           |FROM invoice AS i LEFT OUTER JOIN line_item AS li ON i.id = li.invoice_id""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).foreach(row => {
        row._1.id should equal(1)
        row._1.description should equal("An invoice")
        row._2 should equal(Some(1))
      })
    }
  }
  
  it should "be able to contain RIGHT OUTER joins" in {
    val query: SelectQuery[Tuple2[Option[Invoice], LineItem]] = (
      SELECT(i.*.?, li.*)
      FROM (i RIGHT_OUTER_JOIN li ON i.id == ?(-1)))

    query.queryExpression should equal(
      """|SELECT i.id, i.description, i.image, li.id, li.invoice_id, li.amount, li.ts
           |FROM invoice AS i RIGHT OUTER JOIN line_item AS li ON i.id = ?""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).foreach(row => {
        row._1 should equal (None)
        row._2.id should equal(1)
        row._2.invoice_id should equal(1)
        row._2.amount should equal(56.78)
      })
    }
  }

  it should "be able to contain joins with multiple conditions and negations" in {
    val query: SelectQuery[Tuple2[Invoice, LineItem]] = (
      SELECT(i.*, li.*)
      FROM (i INNER_JOIN li ON i.id == li.invoice_id && NOT(i.id <> ?(5000))))

    query.queryExpression should equal(
      """|SELECT i.id, i.description, i.image, li.id, li.invoice_id, li.amount, li.ts
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id AND NOT (i.id <> ?)""".stripMargin)
  }

  it should "be able to contain projections along with individual columns" in {
    val query: SelectQuery[Tuple2[Invoice, BigDecimal]] = (
      SELECT(i.*, li.amount)
      FROM (i INNER_JOIN li ON i.id == li.invoice_id))

    query.queryExpression should equal(
      """|SELECT i.id, i.description, i.image, li.amount
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id""".stripMargin)
  }

  it should "be able to support distinct queries" in {
    val query: SelectQuery[Tuple2[Invoice, BigDecimal]] = (
      SELECT DISTINCT (i.*, li.amount)
      FROM (i INNER_JOIN li ON i.id == li.invoice_id))

    query.queryExpression should equal(
      """|SELECT DISTINCT i.id, i.description, i.image, li.amount
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id""".stripMargin)
  }

  it should "support where conditions" in {
    val query = (
      SELECT DISTINCT (i.*, li.amount)
      FROM (i INNER_JOIN li ON i.id == li.invoice_id)
      WHERE i.id == ?(1))

    query.queryExpression should equal(
      """|SELECT DISTINCT i.id, i.description, i.image, li.amount
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id
           |WHERE i.id = ?""".stripMargin)
  }

  it should "be buildable in a functional style" in {
    def buildQuery(invoiceId: Long) = (
      SELECT DISTINCT (i.*, li.amount)
      FROM (i INNER_JOIN li ON i.id == li.invoice_id)
      WHERE i.id == ?(invoiceId))

    buildQuery(1).queryExpression should equal(
      """|SELECT DISTINCT i.id, i.description, i.image, li.amount
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id
           |WHERE i.id = ?""".stripMargin)
  }

  it should "support ordering in default, ascending and descending order" in {
    val query = (
      SELECT DISTINCT (i.*, li.amount)
      FROM (i INNER_JOIN li ON i.id == li.invoice_id)
      ORDER_BY (i.id, i.id ASC, li.amount DESC))

    query.queryExpression should equal(
      """|SELECT DISTINCT i.id, i.description, i.image, li.amount
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id
           |ORDER BY i.id, i.id ASC, li.amount DESC""".stripMargin)
  }

  it should "support aggregations" in {
    val query: SelectQuery[Tuple2[Long, BigDecimal]] = (
      SELECT(i.id, MAX(li.amount))
      FROM (i INNER_JOIN li ON i.id == li.invoice_id)
      GROUP_BY (i.id))

    query.queryExpression should equal(
      """|SELECT i.id, MAX(li.amount)
           |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id
           |GROUP BY i.id""".stripMargin)
  }

  it should "support LIMIT and OFFSET clauses" in {
    val query: SelectQuery[Invoice] = (
      SELECT(i.*)
      FROM i
      LIMIT ?(5)
      OFFSET ?(10))

    query.queryExpression should equal(
      """|SELECT i.id, i.description, i.image
           |FROM invoice AS i
           |LIMIT ?
           |OFFSET ?""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).toList.length should equal(0)
    }
  }

  it should "support the use of strings to plug in expressions for unsupported syntax" in {
    val query: SelectQuery[Invoice] = (
      SELECT(i.*)
      FROM i
      WHERE i.id == EXPR("(SELECT MAX(id) FROM line_item)"))

    query.queryExpression should equal(
      """|SELECT i.id, i.description, i.image
           |FROM invoice AS i
           |WHERE i.id = (SELECT MAX(id) FROM line_item)""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).toList.length should equal(1)
    }
  }

  it should "support the use of strings to plug in values for unsupported syntax" in {
    val query: SelectQuery[Tuple2[Long, BigDecimal]] = (
      SELECT(i.id, VEXPR[BigDecimal]("(SELECT MAX(amount) FROM line_item WHERE invoice_id = i.id)"))
      FROM i)

    query.queryExpression should equal(
      """|SELECT i.id, (SELECT MAX(amount) FROM line_item WHERE invoice_id = i.id)
           |FROM invoice AS i""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).toList.length should equal(1)
    }
  }

  it should "support subqueries in the SELECT clause" in {
    val subQuery: SelectQuery[Long] = SELECT(MAX(invoice.id)) FROM invoice

    val query: SelectQuery[Tuple2[Invoice, Long]] = (
      SELECT(i.*, subQuery)
      FROM i)

    query.queryExpression should equal("""|SELECT i.id, i.description, i.image, (SELECT MAX(invoice.id)
                                          |FROM invoice)
                                          |FROM invoice AS i""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).toList.length should equal(1)
    }
  }

  it should "support correlated subqueries in the WHERE clause" in {
    val query: SelectQuery[Invoice] = (
      SELECT(i.*)
      FROM i
      WHERE i.id == (SELECT(MAX(invoice.id)) FROM invoice WHERE invoice.description == i.description))

    query.queryExpression should equal("""|SELECT i.id, i.description, i.image
                                          |FROM invoice AS i
                                          |WHERE i.id = (SELECT MAX(invoice.id)
                                          |FROM invoice
                                          |WHERE invoice.description = i.description)""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn).toList.length should equal(1)
    }
  }

  it should "support an IN clause taking a sequence of bound values" in {
    val query = (
      SELECT(i.id)
      FROM i
      WHERE (i.id IN ?(Seq(1, 2, 3))))
    query.queryExpression should equal("""|SELECT i.id
                                          |FROM invoice AS i
                                          |WHERE i.id IN (?, ?, ?)""".stripMargin)

    db.withConnection { implicit conn =>
      query.toList.length should equal(1)
    }
  }

  it should "support an IN clause taking a sequence of values specified against a bound value" in {
    val seq = Seq(1, 2, 3)
    val query = (
      SELECT(i.id)
      FROM i
      WHERE (?(1) IN ?(seq)))
    query.queryExpression should equal("""|SELECT i.id
                                          |FROM invoice AS i
                                          |WHERE ? IN (?, ?, ?)""".stripMargin)

    db.withConnection { implicit conn =>
      query.toList.length should equal(1)
    }
  }

  it should "support an IN clause taking a query" in {
    val query = (
      SELECT(i.id)
      FROM i
      WHERE (i.id IN (SELECT(invoice.id) FROM invoice)))
    query.queryExpression should equal("""|SELECT i.id
                                          |FROM invoice AS i
                                          |WHERE i.id IN (SELECT invoice.id
                                          |FROM invoice)""".stripMargin)

    db.withConnection { implicit conn =>
      query.toList.length should equal(1)
    }
  }

  it should "support an EXISTS clause taking a query" in {
    val query = (
      SELECT(i.id)
      FROM i
      WHERE EXISTS(SELECT(invoice.id) FROM invoice WHERE invoice.id == i.id))
    query.queryExpression should equal("""|SELECT i.id
                                          |FROM invoice AS i
                                          |WHERE EXISTS (SELECT invoice.id
                                          |FROM invoice
                                          |WHERE invoice.id = i.id)""".stripMargin)

    db.withConnection { implicit conn =>
      query.toList.length should equal(1)
    }
  }

  "A SELECT query" should "allow us to put together aggregations, wheres and all this other stuff and work correctly" in {
    def findLineItemsForInvoice(invoiceId: Long) = (
      SELECT DISTINCT (i.*, li.*, MAX(li.amount) AS "the_max", FN("MIN")(li.amount), ?(5))
      FROM (i INNER_JOIN li ON i.id == li.invoice_id && i.id == li.invoice_id)
      WHERE i.id == ?(invoiceId) && NOT(i.id <> ?(5000))
      ORDER_BY (i.id ASC, li.ts DESC)
      GROUP_BY (i.*, li.id, li.invoice_id, li.ts)
      LIMIT ?(3)
      OFFSET ?(0))

    val query = findLineItemsForInvoice(1)

    query.queryExpression should equal(
      """|SELECT DISTINCT i.id, i.description, i.image, li.id, li.invoice_id, li.amount, li.ts, MAX(li.amount) AS the_max, MIN(li.amount), ?
         |FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id AND i.id = li.invoice_id
         |WHERE i.id = ? AND NOT (i.id <> ?)
         |GROUP BY i.id, i.description, i.image, li.id, li.invoice_id, li.ts
         |ORDER BY i.id ASC, li.ts DESC
         |LIMIT ?
         |OFFSET ?""".stripMargin)

    // Run the query
    db.withConnection { implicit conn =>
      val result: Iterable[(Invoice, LineItem, BigDecimal, BigDecimal, Int)] = query(conn)
      result.foreach { row =>
        {
          row._1.isInstanceOf[Invoice] should be(true)
          row._1.id should equal(1)
          row._1.description should equal("An invoice")
          row._2.isInstanceOf[LineItem] should be(true)
          row._2.id should equal(1)
          row._2.invoice_id should equal(1)
          row._2.amount should equal(56.78)
          row._3 should equal(56.78)
          row._4 should equal(56.78)
          row._5 should equal(5)
        }
      }
    }
  }

  "A SELECT query" should "also support setting binary data" in {
    // Define some query builder methods
    def insertInvoice(invoice: Invoice) = (
      INSERT INTO i(i.description)
      VALUES (?(invoice.description)))

    val imageData = scala.io.Source.fromFile(
      new java.io.File("test_image.jpg"))
      .map(_.toByte)
      .toArray
    val invoice = Invoice(description = "An invoice", image = Some(imageData))
    db.withConnection { implicit conn =>
      insertInvoice(invoice)(conn)
    }
  }

  "A DELETE query" should "support a WHERE clause" in {
    val query = DELETE FROM line_item WHERE line_item.id == ?(-50)

    query.deleteExpression should equal(
      """|DELETE FROM line_item
           |WHERE line_item.id = ?""".stripMargin)

    db.withConnection { implicit conn =>
      query(conn)
    }
  }

  it should "return the number of deleted rows" in {
    db.withConnection { implicit conn =>
      val query = DELETE FROM line_item
      query.deleteExpression should equal("DELETE FROM line_item")
      query.go should equal(1)
    }
  }
  
  it should "support a RETURNING clause" in {
    db.withConnection { implicit conn =>
      val query = DELETE FROM li RETURNING li.id
      query.deleteExpression should equal("""|DELETE FROM line_item AS li
                                             |RETURNING li.id""".stripMargin)
      val deleted:SelectResult[Long] = query go
    }
  }
  
  "The INSERT function" should "be able to use a RETURNING_IDS clause" in {
    // Define some query builder methods
    def insertInvoice(invoice: Invoice) = (
      INSERT INTO i(i.description)
      VALUES (?(invoice.description))
      RETURNING_IDS (i.id))
      
    val invoice = Invoice(description = "An invoice")
    db.withConnection { implicit conn =>
      val query = insertInvoice(invoice)
      query.insertExpression should equal("""|INSERT INTO invoice (description) VALUES(?)""".stripMargin)
      insertedInvoiceId = query go
    }
    insertedInvoiceId should equal(3)
  }

  case class MyType(wrapped: String)

  "Type mappings" should "be extensible" in {
    // The next line won't compile yet and will complain about not finding an implicit value for parameter typeMapping
    // val boundValue = ?(MyType("coolness"))

    // Let's define our own type mapping for MyType
    object MyTypeMappings {
      implicit object MyTypeMapping extends TypeMapping[MyType] {
        def _get(rs: ResultSet, position: Int) = Extraction(MyType(rs.getString(position)), 1)

        def _set(ps: PreparedStatement, position: Int, value: MyType) = ps.setString(position, value.wrapped)
      }

      // Option types require their own type mapping, so let's define that as well
      implicit val OptionMyTypeMapping = new OptionTypeMapping(MyTypeMapping)
    }

    import MyTypeMappings._

    // After defining our implicit type mapping and bringing it into scope, these lines do compile 
    val boundValue = ?(MyType("coolness"))
    val optionValue: Option[MyType] = None
    val boundOptionValue = ?(optionValue)
  }
}