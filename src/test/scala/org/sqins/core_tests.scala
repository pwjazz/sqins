package org.sqins

import java.sql._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import Implicits._

// Define our Scala model
case class Invoice(id: Long, description: String)

case class LineItem(id: Long, invoiceId: Long, amount: BigDecimal, ts: Timestamp)

// Define our database tables
object Invoice extends Table[Invoice]("invoice") {
  val id = Column[Long]("id")
  val description = Column[String]("description")

  columns = Seq(id, description)
}

object LineItem extends Table[LineItem]("line_item") {
  val id = Column[Long]("id")
  val invoice_id = Column[Long]("invoice_id")
  val amount = Column[BigDecimal]("amount")
  val ts = Column[Timestamp]("ts")

  columns = Seq(id, invoice_id, amount, ts)
}

class FirstSpec extends FlatSpec with ShouldMatchers {
  // Set up some aliases (not strictly necessary, but testing here)
  val i = Invoice AS "i"
  val li = LineItem AS "li"

  Class.forName("org.postgresql.Driver");
  
  def findLineItemsForInvoice(invoiceId: Long) = (
    SELECT DISTINCT(i.*, li.*, SQL.MAX(li.amount) AS "the_max", "MIN".call(li.amount))
    FROM (i INNER_JOIN li ON i.id == li.invoice_id && i.id == li.invoice_id)
    WHERE (i.id == Bind(invoiceId) && NOT(i.id <> Bind(5000)))
    ORDER_BY (i.id ASC, li.ts DESC)
    GROUP_BY (i.*, li.id, li.invoice_id, li.ts))

  val url = "jdbc:postgresql://localhost/sqins"
  val props = new java.util.Properties()
  props.setProperty("user", "sqins")
  props.setProperty("password", "sqins")
  def conn = DriverManager.getConnection(url, props);

  var query = findLineItemsForInvoice(1)
  "The query" should "have the correct SQL expression" in {
    query.expression should equal(
      """SELECT DISTINCT i.id, i.description, li.id, li.invoice_id, li.amount, li.ts, MAX(li.amount) AS the_max, MIN(li.amount)
FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id AND i.id = li.invoice_id
WHERE i.id = ? AND NOT i.id <> ?
GROUP BY i.id, i.description, li.id, li.invoice_id, li.ts
ORDER BY i.id ASC, li.ts DESC""")
  }
  
  "The query" should "return a single row with the correct values" in {
    // Run the query
    query(conn).foreach { row =>
      {
        row._1.id should equal(1)
        row._1.description should equal("An invoice")
        row._2.id should equal(1)
        row._2.invoiceId should equal(1)
        row._2.amount should equal(56.78)
        row._3 should equal(56.78)
        row._4 should equal(56.78)
      }
    }
  }
}