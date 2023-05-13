# mysql-json-table

Using mysql to store id-to-json tables.

A table would look something like this:

|     id       |      data       |
|--------------|-----------------|
| `SQL.iden a` | `JSON.encode a` |

## Why would you do this?

To re-use mysql-server capabilities without having to deal with table reshaping.
If changes come down the road, the data content might change, but the table stays the same.
JSON can easily be made compatible between versions, making for smoother releases and rollbacks.
