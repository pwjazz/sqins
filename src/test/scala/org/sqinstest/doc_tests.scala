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

import org.sqins._
import org.sqins.Implicits._

/**
 * Test the syntax of everything listed in the docs.
 */
object DocCompilationText {
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

  var query = SELECT(db.invoice.id) FROM (db.invoice)

  db.withConnection { conn =>
    val result: Iterable[Long] = query(conn);

    // You can only iterate over result while the Connection is still open
    result.foreach { row =>
      println(row * 5)
    }

    // To return everything, you could have done this:
    result.toList

    // The resulting list doesn't depend on the database Connection
  }

  query = SELECT(db.invoice.id) FROM db.invoice

  db.withConnection { implicit conn =>
    val result: Iterable[Long] = query go
  }

  db.withConnection { implicit conn =>
    query.foreach { row =>
      println(row * 5)
    }
  }

  val query2 = SELECT(db.invoice.id, db.invoice.description, db.invoice.image) FROM db.invoice

  db.withConnection { implicit conn =>
    query2.foreach { row: Tuple3[Long, String, Option[Array[Byte]]] =>
      println(row._1 * 5)
      println(row._2 + " more string")
      println(row._3)
    }
  }

  val query3 = SELECT(db.invoice.*) FROM db.invoice

  db.withConnection { implicit conn =>
    query3.foreach { row: Invoice =>
      println(row.id * 5)
      println(row.description + " more string")
      println(row.image)
    }
  }

  val query4 = (
    SELECT(db.i.*, db.li.*)
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

  val query5 = (
    SELECT(db.i.id, db.li.amount)
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id))

  db.withConnection { implicit conn =>
    query5.foreach { row: Tuple2[Long, BigDecimal] =>
      println(row._1)
      println(row._2)
    }
  }

  (SELECT(db.i.id, db.li.amount)
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    WHERE (db.li.amount > db.li.id))

  (SELECT(db.i.id, db.li.amount)
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id && db.li.id <> db.i.id)
    WHERE (db.li.amount > db.li.id || db.li.amount == db.li.amount))

  (SELECT(db.i.id, db.li.amount)
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    WHERE (db.li.amount > db.li.id && NOT(db.li.amount <> db.li.amount)))

  val query6: SelectQuery[Tuple2[Long, BigDecimal]] = (
    SELECT(db.i.id, MAX(db.li.amount))
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    GROUP_BY (db.i.id))

  val invoiceId = 5

  val query7 = (
    SELECT(db.i.id, db.li.amount)
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    WHERE (db.i.id == ?(invoiceId)))

  (SELECT(db.i.id, db.li.amount)
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    WHERE (db.i.id == ?(5) || db.i.id == ?(5 * 35 + 3)))

  // TODO: write actual test for this
  (SELECT(db.i.id, db.li.amount)
    FROM (db.i INNER_JOIN db.li ON EXPR("db.i.id funky_database_operator db.li.invoice_id")))

  (SELECT(db.i.id, VEXPR[BigDecimal]("db.li.amount * 5"))
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id))

  (SELECT(db.i.id)
    FROM db.i
    WHERE FN("lower")(db.i.description) == ?("my lowercase description"))

  val query8: SelectQuery[Tuple2[Long, BigDecimal]] = (
    SELECT(db.i.id, FN("SPECIAL_AGGREGATE")(db.li.amount))
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    GROUP_BY (db.i.id))

  object MyImplicits {
    implicit def SPECIAL_AGGREGATE = FN("SPECIAL_AGGREGATE")
  }

  import MyImplicits._

  val query9: SelectQuery[Tuple2[Long, BigDecimal]] = (
    SELECT(db.i.id, SPECIAL_AGGREGATE(db.li.amount))
    FROM (db.i INNER_JOIN db.li ON db.i.id == db.li.invoice_id)
    GROUP_BY (db.i.id))

  db.withConnection { implicit conn =>
    val insertedKey: Long = INSERT INTO db.invoice(db.invoice.description) VALUES (?("My Description"))
  }

  val newInvoice = Invoice(description = "My Description")

  db.withConnection { implicit conn =>
    val insertedKey: Long = INSERT INTO db.invoice VALUES (newInvoice)
  }

  db.withConnection { implicit conn =>
    val numberOfInsertedRows: Int = (
      INSERT INTO db.invoice(db.invoice.description)
      SELECT (db.invoice.description) FROM db.invoice)
  }

  db.withConnection { implicit conn =>
    val numberOfUpdatedRows: Int = (
      UPDATE(db.invoice)
      SET (db.invoice.description := ?("New description")))
  }

  val updatedInvoice = Invoice(id = 5, description = "New description")

  db.withConnection { implicit conn =>
    val numberOfUpdatedRows = (
      UPDATE(db.invoice)
      SET (updatedInvoice))
  }

  (UPDATE(db.invoice)
    SET (db.invoice.description := db.invoice.description || ?(" with additional text")))

  db.withConnection { implicit conn =>
    val numberOfUpdatedRows: Int = (
      UPDATE(db.invoice)
      SET (db.invoice.description := ?("New description"))
      WHERE (db.invoice.id <= ?(5)))
  }

  db.withConnection { implicit conn =>
    val numberOfUpdatedRows: Int = (
      DELETE FROM db.invoice
      WHERE (db.invoice.id <= ?(5)))
  }

  (
    SELECT(db.i.id, (SELECT(MAX(db.li.id)) FROM db.li WHERE db.li.invoice_id == db.i.id))
    FROM (db.i)
    WHERE (db.i.id == (SELECT(MAX(db.li.invoice_id)) FROM db.li)))

  db.withConnection { implicit conn =>
    val numberOfUpdatedRows: Int = SQL("DELETE FROM invoice WHERE id <= ?", ?(5)).executeUpdate
  }

  db.withConnection { implicit conn =>
    val result: java.sql.ResultSet = SQL("SELECT * FROM invoice").executeQuery
  }
  
  db.withConnection { implicit conn =>
    val query = (
        SELECT (db.i.*)
        FROM db.i
        WHERE (db.i.id IN ?(Seq(1, 2, 3))))
  }
  
  db.withConnection { implicit conn =>
    val query = (
      SELECT(db.i.*)
      FROM (db.i)
      WHERE EXISTS (SELECT(db.i2.id)
        FROM (db.i2)
        WHERE db.i.id == db.i2.id))
  }
}