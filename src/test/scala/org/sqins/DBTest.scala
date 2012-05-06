package org.sqins

trait DBTest {
  def conn = {
    import java.sql.DriverManager
    Class.forName("org.postgresql.Driver");
    val url = "jdbc:postgresql://localhost/sqins"
    val props = new java.util.Properties()
    props.setProperty("user", "sqins")
    props.setProperty("password", "sqins")
    DriverManager.getConnection(url, props);
  }

  def initSchema() = SQL("""
        DROP TABLE IF EXISTS line_item;
        DROP TABLE IF EXISTS invoice;
        
        create table invoice (
          id SERIAL,
          description VARCHAR(255),
          primary key(id));
        
        create table line_item (
          id SERIAL,
          invoice_id BIGINT,
          amount DECIMAL(22,2),
          ts TIMESTAMP DEFAULT NOW(),
          primary key(id)); 
        
        alter table line_item add constraint fk_line_item_invoice  foreign key (invoice_id) references invoice(id);
        """).executeUpdate(conn)
}