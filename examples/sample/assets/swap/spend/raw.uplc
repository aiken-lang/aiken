(program
  1.0.0
  (lam
    datum
    (lam
      _
      (lam
        _
        (force
          [
            [
              [
                (force (builtin ifThenElse))
                [
                  (lam
                    __constr_fields_exposer
                    [
                      (lam
                        __constr_get_field
                        [
                          (lam
                            x
                            [
                              (lam
                                __constr_name_8
                                [
                                  (lam
                                    __subject_name_7
                                    (force
                                      [
                                        [
                                          [
                                            (force (builtin ifThenElse))
                                            [
                                              [
                                                (builtin equalsInteger)
                                                __subject_name_7
                                              ]
                                              (con integer 0)
                                            ]
                                          ]
                                          (delay
                                            [
                                              (lam
                                                __constr_fields_18
                                                [
                                                  (lam
                                                    signer
                                                    [
                                                      [
                                                        (builtin
                                                          equalsByteString
                                                        )
                                                        signer
                                                      ]
                                                      (con bytestring #0303ff)
                                                    ]
                                                  )
                                                  [
                                                    (builtin unBData)
                                                    [
                                                      (force (builtin headList))
                                                      __constr_fields_18
                                                    ]
                                                  ]
                                                ]
                                              )
                                              [
                                                __constr_fields_exposer
                                                __constr_name_8
                                              ]
                                            ]
                                          )
                                        ]
                                        (delay
                                          (force
                                            [
                                              [
                                                [
                                                  (force (builtin ifThenElse))
                                                  [
                                                    [
                                                      (builtin equalsInteger)
                                                      __subject_name_7
                                                    ]
                                                    (con integer 1)
                                                  ]
                                                ]
                                                (delay
                                                  [
                                                    (lam
                                                      __constr_fields_17
                                                      [
                                                        (lam
                                                          signer
                                                          [
                                                            [
                                                              (builtin
                                                                equalsByteString
                                                              )
                                                              signer
                                                            ]
                                                            (con
                                                              bytestring
                                                              #0303ff
                                                            )
                                                          ]
                                                        )
                                                        [
                                                          (builtin unBData)
                                                          [
                                                            (force
                                                              (builtin headList)
                                                            )
                                                            __constr_fields_17
                                                          ]
                                                        ]
                                                      ]
                                                    )
                                                    [
                                                      __constr_fields_exposer
                                                      __constr_name_8
                                                    ]
                                                  ]
                                                )
                                              ]
                                              (delay (con bool False))
                                            ]
                                          )
                                        )
                                      ]
                                    )
                                  )
                                  [
                                    (force (force (builtin fstPair)))
                                    [ (builtin unConstrData) __constr_name_8 ]
                                  ]
                                ]
                              )
                              x
                            ]
                          )
                          [
                            [
                              (lam
                                signer
                                (lam
                                  amount
                                  [
                                    (builtin constrData)
                                    [
                                      [ (builtin mkPairData) (con data #00) ]
                                      [
                                        (builtin listData)
                                        [
                                          [ (force (builtin mkCons)) signer ]
                                          [
                                            [ (force (builtin mkCons)) amount ]
                                            (con data #9fff)
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                )
                              )
                              (con bytestring #0404ff)
                            ]
                            (con integer 1000)
                          ]
                        ]
                      )
                      (lam
                        __constr_list
                        (lam
                          __arg_number
                          [
                            (lam
                              __recurse
                              [
                                [ [ __recurse __recurse ] (con integer 0) ]
                                __constr_list
                              ]
                            )
                            (lam
                              __self_recursor
                              (lam
                                __current_arg_number
                                (lam
                                  __list_of_constr_args
                                  [
                                    [
                                      [
                                        [
                                          (force (builtin ifThenElse))
                                          [
                                            [
                                              (builtin equalsInteger)
                                              __arg_number
                                            ]
                                            __current_arg_number
                                          ]
                                        ]
                                        (force (builtin headList))
                                      ]
                                      (lam
                                        __current_list_of_constr_args
                                        [
                                          [
                                            [ __self_recursor __self_recursor ]
                                            [
                                              [
                                                (builtin addInteger)
                                                __current_arg_number
                                              ]
                                              (con integer 1)
                                            ]
                                          ]
                                          [
                                            (force (builtin tailList))
                                            __current_list_of_constr_args
                                          ]
                                        ]
                                      )
                                    ]
                                    __list_of_constr_args
                                  ]
                                )
                              )
                            )
                          ]
                        )
                      )
                    ]
                  )
                  (lam
                    __constr_var
                    [
                      (force (force (builtin sndPair)))
                      [ (builtin unConstrData) __constr_var ]
                    ]
                  )
                ]
              ]
              (delay (con unit ()))
            ]
            (delay (error ))
          ]
        )
      )
    )
  )
)