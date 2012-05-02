package org.sqins

import java.sql._
import org.scalatest.FlatSpec
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
  val invoiceId = Column[Long]("invoice_id")
  val amount = Column[BigDecimal]("amount")
  val time = Column[Timestamp]("ts")

  columns = Seq(id, invoiceId, amount, time)
}

class FirstSpec extends FlatSpec {
  // Set up some aliases (not strictly necessary, but testing here)
  val i = Invoice AS "i"
  val li = LineItem AS "li"

  Class.forName("org.postgresql.Driver");

  def findLineItemsForInvoice(invoiceId: Long) = (
    SELECT(i.*, li.*)
    FROM (i INNER_JOIN li ON i.id == li.invoiceId && i.id == li.invoiceId)
    WHERE (i.id == invoiceId)
    ORDER_BY (i.id ASC, li.time DESC))

  "A query" should "run" in {
    val url = "jdbc:postgresql://localhost/sqins"
    val props = new java.util.Properties()
    props.setProperty("user", "sqins")
    props.setProperty("password", "sqins")
    def conn = DriverManager.getConnection(url, props);

    // Build the query
    val query = findLineItemsForInvoice(1)

    // Debug SQL
    info(query.toString)
    info(query.select.toString)
    info(query.from.toString)
    info(query.where.toString)

    // Run the query
    query(conn).foreach { row =>
      {
        // Print each row
        info(row.toString)
      }
    }
  }
}