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
    SELECT DISTINCT(i.*, li.*, MAX(li.amount) AS "the_max", FN("MIN")(li.amount))
    FROM (i INNER_JOIN li ON i.id == li.invoice_id && i.id == li.invoice_id)
    WHERE (i.id == ?(invoiceId) && NOT(i.id <> ?(5000)))
    ORDER_BY (i.id ASC, li.ts DESC)
    GROUP_BY (i.*, li.id, li.invoice_id, li.ts))

  val url = "jdbc:postgresql://localhost/sqins"
  val props = new java.util.Properties()
  props.setProperty("user", "sqins")
  props.setProperty("password", "sqins")
  val conn = DriverManager.getConnection(url, props);

  var query = findLineItemsForInvoice(1)
  "The query" should "have the correct SQL expression" in {
    query.expression should equal(
      """SELECT DISTINCT i.id, i.description, li.id, li.invoice_id, li.amount, li.ts, MAX(li.amount) AS the_max, MIN(li.amount)
FROM invoice AS i INNER JOIN line_item AS li ON i.id = li.invoice_id AND i.id = li.invoice_id
WHERE i.id = ? AND NOT i.id <> ?
GROUP BY i.id, i.description, li.id, li.invoice_id, li.ts
ORDER BY i.id ASC, li.ts DESC""")
  }
  
  "A single-table query" should "return an Iterable of the single row type" in {
      (SELECT (i.*) FROM i)(conn).isInstanceOf[Iterable[Invoice]]
  }
  
  "The query" should "return a single row with the correct values" in {
    // Run the query
    val result: Iterable[(Invoice, LineItem, BigDecimal, BigDecimal)] = query(conn)
    result.foreach { row =>
      {
        row._1.isInstanceOf[Invoice] should be(true)
        row._1.id should equal(1)
        row._1.description should equal("An invoice")
        row._2.isInstanceOf[LineItem] should be(true)
        row._2.id should equal(1)
        row._2.invoiceId should equal(1)
        row._2.amount should equal(56.78)
        row._3 should equal(56.78)
        row._4 should equal(56.78)
      }
    }
  }
}