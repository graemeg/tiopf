connect  'OrderSystem.fdb' user 'SYSDBA' password 'masterkey';

insert into customers (oid, cu_firstname, cu_lastname, cu_phone)
values ('1001', 'Tom', 'Jones', '555-1111');

insert into customers (oid, cu_firstname, cu_lastname, cu_phone)
values ('1002', 'Graeme', 'Geldenhuys', '555-2222');



insert into products (oid, pr_prodname, pr_unitlistprice, pr_unitsinstock)
values ('2001', 'Product A', '10000', 8);

insert into products (oid, pr_prodname, pr_unitlistprice, pr_unitsinstock)
values ('2002', 'Product B', '8000', 13);

insert into products (oid, pr_prodname, pr_unitlistprice, pr_unitsinstock)
values ('2003', 'Product C', '1000', 55);

insert into products (oid, pr_prodname, pr_unitlistprice, pr_unitsinstock)
values ('2004', 'Product D', '25000', 2);



insert into orders (oid, or_oid_customer, or_orderdate, or_soldby)
values ('3001', '1001', '20150120T143200', 'Mark');

insert into orders (oid, or_oid_customer, or_orderdate, or_soldby)
values ('3002', '1002', '20150204T102300', 'Mark');

insert into orders (oid, or_oid_customer, or_orderdate, or_soldby)
values ('3003', '1002', '20150213T164000', 'Mark');



insert into orderlines (oid, ol_oid_orders, ol_oid_products, ol_quantity, ol_unitsaleprice)
values ('4001', '3001', '2001', 1, '10000');

insert into orderlines (oid, ol_oid_orders, ol_oid_products, ol_quantity, ol_unitsaleprice)
values ('4002', '3001', '2003', 5, '950');

insert into orderlines (oid, ol_oid_orders, ol_oid_products, ol_quantity, ol_unitsaleprice)
values ('4003', '3002', '2002', 1, '8000');

insert into orderlines (oid, ol_oid_orders, ol_oid_products, ol_quantity, ol_unitsaleprice)
values ('4004', '3002', '2001', 1, '10000');

insert into orderlines (oid, ol_oid_orders, ol_oid_products, ol_quantity, ol_unitsaleprice)
values ('4005', '3003', '2004', 1, '25000');


commit;

exit;
