create table invoice (
  id SERIAL,
  description VARCHAR(255),
  primary key(id));

create table line_item (
  id SERIAL,
  invoice_id BIGINT,
  amount MONEY,
  ts TIMESTAMP,
  primary key(id)); 

alter table line_item add constraint fk_line_item_invoice  foreign key (invoice_id) references invoice(id);