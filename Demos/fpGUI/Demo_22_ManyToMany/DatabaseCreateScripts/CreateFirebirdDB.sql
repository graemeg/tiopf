Create Database 'OrderSystem.fdb' user 'SYSDBA' password 'masterkey';

-- Connect 'OrderSystem.fdb' user 'SYSDBA' password 'masterkey';

CREATE DOMAIN D_OID AS VARCHAR(36);
CREATE DOMAIN D_CURRENCY AS INTEGER DEFAULT 0;
CREATE DOMAIN D_TIMESTAMP AS CHAR(15);

create table customers (
	oid D_OID not null,
	cu_firstname varchar(60) not null,
	cu_lastname varchar(60) not null,
	cu_phone varchar(25),
    CONSTRAINT customers_pk PRIMARY KEY (OID),
    CONSTRAINT customers_un UNIQUE (cu_firstname, cu_lastname)
	);


create table orders (
	oid D_OID not null,
	or_oid_customer D_OID not null,
	or_orderdate D_TIMESTAMP not null,
	or_soldby varchar(30),
    CONSTRAINT orders_pk PRIMARY KEY (OID),
    CONSTRAINT orders_customers_fk foreign key (or_oid_customer) references customers
	);


create table orderlines (
	oid D_OID not null,
	ol_oid_orders D_OID not null,
	ol_oid_products D_OID not null,
	ol_quantity integer not null,
	ol_unitsaleprice D_CURRENCY not null,
    CONSTRAINT orderlines_pk PRIMARY KEY (OID),
    CONSTRAINT orderlines_orders_fk foreign key (ol_oid_orders) references orders
	);
	

create table products (
	oid D_OID not null,
	pr_prodname varchar(100) not null,
	pr_unitlistprice D_CURRENCY not null,
	pr_unitsinstock integer not null,
    CONSTRAINT products_pk PRIMARY KEY (OID),
    CONSTRAINT products_un UNIQUE (pr_prodname)
	);


alter table orderlines add constraint orderlines_products_fk foreign key (ol_oid_products) references products;


exit;
