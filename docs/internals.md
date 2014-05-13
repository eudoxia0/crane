% Crane Internals
% Fernando Borretti

# Tables

When a table is created, its digest is separated into two parts: Columns, and
constraints. Columns are basically a map of slot names to SQL types. Constraints
are automatically-named SQL constraints created by extracting slot options. That
is, no constraints from the `deftable` macro are nameless, they are all named
and at the column level.

Consider the following table:

```lisp
(deftable user
  (name :type string :pk t :nullp nil)
  (age :type integer :nullp nil))
```

A human would write SQL like the following:

```sql
CREATE TABLE user (
  name STRING NOT NULL PRIMARY KEY,
  age INTEGER NOT NULL
)
```

Crane, however, would generate the following SQL:

```sql
CREATE TABLE user (
  name STRING CONSTRAINT user_name_nullity NOT NULL
              CONSTRAINT user_name_primary PRIMARY KEY,
  age INTEGER CONSTRAINT user_age_nullity NOT NULL
)
```

Naming all constraints makes it possible for them to be dropped simply through
`ALTER TABLE` statements when migrating from an old schema.
