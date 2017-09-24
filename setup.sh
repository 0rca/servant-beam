#!/bin/sh

sqlite3 shoppingcart.db "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
