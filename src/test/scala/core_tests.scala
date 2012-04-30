package org.sqins

import java.sql._
import org.scalatest.FlatSpec
import Implicits._

// Define our Scala model
case class Invoice(id: Long, description: String)
case class LineItem(id: Long, invoiceId: Long, amount: BigDecimal)

// Define our database tables
object Invoice extends Table[Invoice]("invoice") {
  val id = Column[Long]("id")
  val description = Column[String]("description")
}

object LineItem extends Table[LineItem]("line_item") {
  val id = Column[Long]("id")
  val invoiceId = Column[Long]("invoice_id")
  val amount = Column[BigDecimal]("amount")
  val time = Column[Timestamp]("ts")
}

class FirstSpec extends FlatSpec {
  // Set up some aliases (not strictly necessary, but testing here)
  val i = Invoice AS "i"
  val li = LineItem AS "li"

  def findLineItemsForInvoice(invoiceId: Long): SelectStatement = (
        SELECT (i.*, li.amount)
        FROM (i INNER_JOIN li ON i.id == li.invoiceId && i.id <> li.invoiceId)
        WHERE (i.id == invoiceId)
        ORDER_BY (i.id ASC, li.time DESC)      
  )
        
  "A query" should "run" in {
    // Set up aliases for use in our query (not strictly necessary)
    
    val conn:Connection = null
    
    val selectClause = (i.*, li.amount)
    
    val fromClause = i :: li
    
    val alternateFromClause = i INNER_JOIN li ON i.id == li.invoiceId && i.id <> li.invoiceId
    
    
  }
}