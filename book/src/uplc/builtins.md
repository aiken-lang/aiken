# Builtins

| Builtins                              | Type Args  | Term Args                                           | Result                |
| ---                                   | ---        | ---                                                 | ---                   |
| `ifThenElse`[^1]                      | \\(α\\)    | (bool, \\(α\\), \\(α\\))                            | \\(α\\)               |
| ---                                   | ---        | ---                                                 | ---                   |
| `addInteger`                          | \-         | (integer, integer)                                  | integer               |
| `subtractInteger`                     | \-         | (integer, integer)                                  | integer               |
| `multiplyInteger`                     | \-         | (integer, integer)                                  | integer               |
| `divideInteger`                       | \-         | (integer, integer)                                  | integer               |
| `modInteger`                          | \-         | (integer, integer)                                  | integer               |
| `quotientInteger`                     | \-         | (integer, integer)                                  | integer               |
| `remainderInteger`                    | \-         | (integer, integer)                                  | integer               |
| `equalsInteger`                       | \-         | (integer, integer)                                  | bool                  |
| `lessThanInteger`                     | \-         | (integer, integer)                                  | bool                  |
| `lessThanEqualsInteger`               | \-         | (integer, integer)                                  | bool                  |
| ---                                   | ---        | ---                                                 | ---                   |
| `appendString`                        | \-         | (string, string)                                    | string                |
| `emptyString`                         | \-         | (string)                                            | bool                  |
| `equalsString`                        | \-         | (string, string)                                    | bool                  |
| `encodeUtf8`                          | \-         | (string)                                            | bytestring            |
| ---                                   | ---        | ---                                                 | ---                   |
| `appendByteString`                    | \-         | (bytestring, bytestring)                            | bytestring            |
| `consByteString`                      | \-         | (integer, bytestring)                               | bytestring            |
| `indexByteString`                     | \-         | (bytestring, integer)                               | integer               |
| `sliceByteString`                     | \-         | (integer, integer, bytestring)                      | bytestring            |
| `lengthOfByteString`                  | \-         | (bytestring)                                        | integer               |
| `equalsByteString`                    | \-         | (bytestring, bytestring)                            | bool                  |
| `lessThanByteString`                  | \-         | (bytestring, bytestring)                            | bool                  |
| `lessThanEqualsByteString`            | \-         | (bytestring, bytestring)                            | bool                  |
| `decodeUtf8`                          | \-         | (bytestring)                                        | string                |
| ---                                   | ---        | ---                                                 | ---                   |
| `chooseData`[^2]                      | \\(α\\)    | (data, \\(α\\), \\(α\\), \\(α\\), \\(α\\), \\(α\\)) | \\(α\\)               |
| `constrData`                          | \-         | (integer, list data)                                | data                  |
| `unConstrData`                        | \-         | data                                                | (integer, list data)  |
| `iData`                               | \-         | (integer)                                           | data                  |
| `unIData`                             | \-         | (data)                                              | integer               |
| `bData`                               | \-         | (bytestring)                                        | data                  |
| `unBData`                             | \-         | (data)                                              | bytestring            |
| `mapData`                             | \-         | (list (pair data data))                             | data                  |
| `unMapData`                           | \-         | (data)                                              | list (pair data data) |
| `listData`                            | \-         | (list data)                                         | data                  |
| `unListData`                          | \-         | (data)                                              | list data             |
| `equalsData`                          | \-         | (data, data)                                        | bool                  |
| `serialiseData`                       | \-         | (data)                                              | bytestring            |
| ---                                   | ---        | ---                                                 | ---                   |
| `chooseList`[^3]                      | \\(α, β\\) | (list \\(α\\), \\(β\\), \\(β\\))                    | \\(β\\)               |
| `mkNilData`                           | \\(α\\)    | (unit)                                              | list \\(α\\)          |
| `mkCons`                              | \\(α\\)    | (\\(α\\), list \\(α\\))                             | list \\(α\\)          |
| `headList`                            | \\(α\\)    | (list \\(α\\))                                      | \\(α\\)               |
| `tailList`                            | \\(α\\)    | (list \\(α\\))                                      | list \\(α\\)          |
| `nullList`                            | \\(α\\)    | (list \\(α\\))                                      | bool                  |
| ---                                   | ---        | ---                                                 | ---                   |
| `mkPairData`                          | \\(α, β\\) | (\\(α\\), \\(β\\))                                  | pair \\(α\\) \\(β\\)  |
| `fstPair`                             | \\(α, β\\) | pair \\(α\\) \\(β\\)                                | \\(α\\)               |
| `sndPair`                             | \\(α, β\\) | pair \\(α\\) \\(β\\)                                | \\(β\\)               |
| ---                                   | ---        | ---                                                 | ---                   |
| `sha2_256`                            | \-         | (bytestring)                                        | bytestring            |
| `sha3_256`                            | \-         | (bytestring)                                        | bytestring            |
| `blake2b_256`                         | \-         | (bytestring)                                        | bytestring            |
| ---                                   | ---        | ---                                                 | ---                   |
| `verifyEd25519Signature`[^4]          | \-         | (bytestring, bytestring, bytestring)                | bytestring            |
| `verifyEcdsaSecp256k1Signature`[^4]   | \-         | (bytestring, bytestring, bytestring)                | bytestring            |
| `verifySchnorrSecp256k1Signature`[^4] | \-         | (bytestring, bytestring, bytestring)                | bytestring            |
| ---                                   | ---        | ---                                                 | ---                   |
| `error`                               | \\(α\\)    | (unit)                                              | \\(α\\)               |
| `trace`                               | \\(α\\)    | (string, \\(α\\))                                   | \\(α\\)               |

[^1]: Returns the second argument when the predicate is `True`, and the third argument when `False`.

[^2]: Each argument corresponds to each of the constructors of a builtin data (in this order): constr, map, list, integer and bytestring. The evaluation will continue with whatever branch actually corresponds to the given term value.

[^3]: Returns the second argument when the list is empty, and the third argument otherwise.

[^4]: Arguments are respectively: the public key, the message and the signature
