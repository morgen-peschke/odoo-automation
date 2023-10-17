Odoo Automation
===============

Automation helper to create pickings and moves in an [Odoo](https://www.odoo.com) Inventory module based on a template.

Running
-------

At the moment, you'll need the build too [`mill`](https://mill-build.com/mill/Intro_to_Mill.html) to run this tool. 
This may change later.

Some of the configuration is handled through environment variables, and something like [`direnv`](https://direnv.net/)
is easiest way I've found to manage this sort of thing, and I highly recommend it. The example 
[`envrc`](./envrc-example) provided is a good jumping-off point for your own config.

Examples
--------

### Logging in

```bash
mill odoo.run login
```

### Getting field information about the `stock.location` model

```bash
mill odoo.run fields --model stock.location --attribute default
```

### Listing the name and id of all active locations

```bash
mill odoo.run search --model stock.location --condition '["active","=","true"]' --fields name,complete_name
```

### Creating pickings from a template

```bash
mill odoo.run pickings --template file:template.json --known-ids file:known.ids.json
```

It's also possible to override the date with `--override-date` and restrict the times of day to control which pickings
are created, which can be very handy for catching up on a missed run or testing.

```bash
mill odoo.run pickings \
  --template file:template.json \
  --known-ids file:known.ids.json \
  --time-of-day pm \
  --override-date 2023-01-01
```

#### `--known-ids`

This allows using the text names of models important to pickings and moves, rather than having to use the 
numeric ids. 

While this can be curated by hand, the recommended approach is to generate it.

```bash
mill odoo.run generate-known-ids > known.ids.json
```

After doing this, it's generally a good idea to make sure none of the names in your template are out of date. The 
easiest way to do this is doing a dry-run.

```bash
ODOO_DRY_RUN=enabled mill odoo.run pickings --template file:template.json --known-ids file:known.ids.json
```

#### `--template`

This provides the pattern for creating pickings and moves. The format is straightforward, with a few bits that can help
make writing them easier.

A very simple example for a daily morning delivery might look like this: 
```json
[
  {
    "person": "Jane Doe",
    "pickingNamePrefix": "JANE/TAKE",
    "pickingName": "TAKE/{{today}}/Jane/{{timeOfDay}}/{{index}}",
    "pickings": [
      {
        "frequency": "daily",
        "timeOfDay": "morning",
        "move_type": "direct",
        "picking_type_id": "Jane: Deliveries",
        "location_id": "JANE/Stock/Ready",
        "location_dest_id": "Partners/Janet",
        "partner_id": "Janet Doe",
        "moves": [
          {"product_id": "Foo", "product_uom_qty": 1}
        ]
      }
    ]
  }
]
```

When there are multiple pickings, fields can be moved to the parent as a default:
```json
[
  {
    "person": "Jane Doe",
    "common": {
      "pickingNamePrefix": "JANE/TAKE",
      "pickingName": "TAKE/{{today}}/Jane/{{timeOfDay}}/{{index}}",
      "move_type": "direct",
      "picking_type_id": "Jane: Deliveries",
      "location_id": "JANE/Stock/Ready",
      "location_dest_id": "Partners/Janet/Home",
      "partner_id": "Janet Doe"
    },
    "pickings": [
      {
        "frequency": "daily",
        "timeOfDay": "morning",
        "moves": [
          {"product_id": "Foo", "product_uom_qty": 1}
        ]
      },
      {
        "frequency": "daily",
        "timeOfDay": "noon",
        "location_dest_id": "Partners/Janet/Work",
        "moves": [
          {"product_id": "Foo", "product_uom_qty": 1}
        ]
      },
      {
        "frequency": "daily",
        "timeOfDay": "night",
        "moves": [
          {"product_id": "Foo", "product_uom_qty": 1}
        ]
      }
    ]
  }
]
```

When everything in a location needs to be moved, this can be specified:
```json
[
  {
    "person": "Jane Doe",
    "pickingNamePrefix": "JANE/TAKE",
    "pickingName": "TAKE/{{today}}/Jane/{{timeOfDay}}/{{index}}",
    "pickings": [
      {
        "frequency": "daily",
        "timeOfDay": "morning",
        "move_type": "direct",
        "picking_type_id": "Jane: Deliveries",
        "location_id": "JANE/Stock/Ready",
        "location_dest_id": "Partners/Janet",
        "partner_id": "Janet Doe",
        "moves": "all"
      }
    ]
  }
]
```

##### `frequency`

This controls if a picking will be created or skipped, and can be any of the following:

A list of days (case-insensitive, and may be the full name as well as the abbreviation):
```json
["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
```

`daily`, which is equivalent to:
```json
["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
```

`weekdays`, which is equivalent to:
```json
["Mon", "Tue", "Wed", "Thu", "Fri"]
```

`weekends`, which is equivalent to:
```json
["Sun", "Sat"]
```

##### `timeOfDay`

This adds a suffix to the generated name, and can be used to skip creating pickings 
(though this is not the default behavior).

This is a case-insensitive value, and can be `"Morning"` or `"AM"`, `"Noon"`, and `"Night"` or `"PM"`.

