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

package org.sqins.test

import java.sql.DriverManager

import org.sqins._
import org.sqins.Implicits._

/**
 * Our test database definition.
 */
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

  /**
   * This initializes our database schema
   */
  def initSchema = SQL("""
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
          ts TIMESTAMP DEFAULT NOW(),
          primary key(id)); 
        
        alter table line_item add constraint fk_line_item_invoice  foreign key (invoice_id) references invoice(id);
        """)
}
