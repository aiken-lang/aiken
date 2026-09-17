use aiken/builtin

const zero: Value = {}

const policy_00 = #"00000000000000000000000000000000000000000000000000000000"

const policy_ff = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const value_00_foo_1 = {
  #"00000000000000000000000000000000000000000000000000000000": {
    "foo": 1,
  },
}

const value_00_foo_2 = {
  #"00000000000000000000000000000000000000000000000000000000": {
    "foo": 2,
  },
}

const value_00_foo_1_ff_foo_1 = {
  #"00000000000000000000000000000000000000000000000000000000": {
    "foo": 1,
  },
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff": {
    "foo": 1,
  },
}

const value_ff_foo_1 = {
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff": {
    "foo": 1,
  },
}

const value_list = [value_00_foo_1, value_00_foo_2]

test equality_symmetry() {
  and {
    zero == {},
    {} == zero,
  }
}

test insert_lookup_value() {
  and {
    builtin.lookup_value(policy_00, "foo", value_00_foo_1) == 1,
    builtin.lookup_value(policy_00, "bar", value_00_foo_1) == 0,
    builtin.lookup_value(policy_ff, "foo", value_00_foo_1) == 0,
  }
}

test scale_value() {
  let value = {
    #"00000000000000000000000000000000000000000000000000000000": {
      "bar": 1,
      "baz": -1,
      "foo": 2,
    },
  }

  and {
    builtin.scale_value(2, value) == {
      #"00000000000000000000000000000000000000000000000000000000": {
        "bar": 2,
        "baz": -2,
        "foo": 4,
      },
    },
    builtin.scale_value(-1, value) == {
      #"00000000000000000000000000000000000000000000000000000000": {
        "bar": -1,
        "baz": 1,
        "foo": -2,
      },
    },
    builtin.scale_value(0, value) == {},
  }
}

test value_contains() {
  and {
    builtin.contains_value({}, {})?,
    (!builtin.contains_value({}, value_00_foo_1))?,
    builtin.contains_value(value_00_foo_1, {})?,
    (!builtin.contains_value(value_00_foo_1, value_00_foo_2))?,
    builtin.contains_value(value_00_foo_2, value_00_foo_1)?,
    builtin.contains_value(value_00_foo_1_ff_foo_1, value_00_foo_1)?,
    builtin.contains_value(value_00_foo_1_ff_foo_1, value_ff_foo_1)?,
    (!builtin.contains_value(value_00_foo_1, value_ff_foo_1))?,
    (!builtin.contains_value(value_ff_foo_1, value_00_foo_1))?,
  }
}

test value_union() {
  and {
    (builtin.union_value({}, {}) == {})?,
    (builtin.union_value(value_00_foo_1, {}) == value_00_foo_1)?,
    (builtin.union_value({}, value_00_foo_1) == value_00_foo_1)?,
    (builtin.union_value(value_00_foo_1, value_00_foo_1) == value_00_foo_2)?,
    (builtin.union_value(value_00_foo_1, value_00_foo_2) != value_00_foo_1)?,
    (builtin.union_value(value_ff_foo_1, value_00_foo_1) == value_00_foo_1_ff_foo_1)?,
    (builtin.union_value(value_00_foo_1, value_ff_foo_1) == value_00_foo_1_ff_foo_1)?,
    (builtin.union_value(
      value_00_foo_1,
      builtin.scale_value(-1, value_00_foo_1),
    ) == {})?,
  }
}

test data() {
  let empty_map: Pairs<Data, Data> = []
  let empty_list: List<Data> = []
  and {
    builtin.value_data({}) == empty_map,
    builtin.value_data({}) != empty_list,
    builtin.un_value_data(empty_map) == {},
    builtin.value_data(value_00_foo_1) == [Pair(policy_00, [Pair("foo", 1)])],
    builtin.un_value_data([Pair(policy_ff, [Pair("foo", 1)])]) == value_ff_foo_1,
  }
}

test non_data() fail {
  let empty_list: List<Data> = []
  builtin.un_value_data(empty_list) |> tautology
}

test list() {
  and {
    value_list == [value_00_foo_1, value_00_foo_2],
    [value_00_foo_1, value_00_foo_2] == value_list,
    builtin.head_list(value_list) == value_00_foo_1,
    builtin.tail_list(value_list) == [value_00_foo_2],
    when value_list is {
      [head, ..] -> head == value_00_foo_1
      _ -> fail
    },
    {
      expect [head, ..] = value_list
      head == value_00_foo_1
    },
    {
      expect [first, second] = value_list
      and {
        first == value_00_foo_1,
        second == value_00_foo_2,
      }
    },
    {
      expect [_, ..tail] = value_list
      tail == [value_00_foo_2]
    },
  }
}
