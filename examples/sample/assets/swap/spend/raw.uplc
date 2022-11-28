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
                            __constr_name_1
                            [
                              (lam
                                __subject_name_0
                                (force
                                  [
                                    [
                                      [
                                        (force (builtin ifThenElse))
                                        [
                                          [
                                            (builtin equalsInteger)
                                            __subject_name_0
                                          ]
                                          (con integer 0)
                                        ]
                                      ]
                                      (delay
                                        [
                                          (lam
                                            __constr_fields_5
                                            [
                                              (lam
                                                price
                                                [
                                                  (lam
                                                    __tail_0_3
                                                    [
                                                      (lam
                                                        __tail_1_4
                                                        [
                                                          (lam
                                                            t
                                                            [
                                                              [
                                                                (builtin
                                                                  lessThanInteger
                                                                )
                                                                (con integer 0)
                                                              ]
                                                              price
                                                            ]
                                                          )
                                                          [
                                                            (builtin unIData)
                                                            [
                                                              (force
                                                                (builtin
                                                                  headList
                                                                )
                                                              )
                                                              __tail_1_4
                                                            ]
                                                          ]
                                                        ]
                                                      )
                                                      [
                                                        (force
                                                          (force
                                                            (builtin tailList)
                                                          )
                                                        )
                                                        __tail_0_3
                                                      ]
                                                    ]
                                                  )
                                                  [
                                                    (force (builtin tailList))
                                                    __constr_fields_5
                                                  ]
                                                ]
                                              )
                                              [
                                                (builtin unIData)
                                                [
                                                  (force (builtin headList))
                                                  __constr_fields_5
                                                ]
                                              ]
                                            ]
                                          )
                                          [
                                            __constr_fields_exposer
                                            __constr_name_1
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
                                                  __subject_name_0
                                                ]
                                                (con integer 2)
                                              ]
                                            ]
                                            (delay
                                              [
                                                (lam
                                                  __constr_fields_2
                                                  [
                                                    (lam
                                                      less
                                                      [
                                                        [
                                                          (builtin
                                                            lessThanInteger
                                                          )
                                                          less
                                                        ]
                                                        (con integer 0)
                                                      ]
                                                    )
                                                    [
                                                      (builtin unIData)
                                                      [
                                                        (force
                                                          (builtin headList)
                                                        )
                                                        __constr_fields_2
                                                      ]
                                                    ]
                                                  ]
                                                )
                                                [
                                                  __constr_fields_exposer
                                                  __constr_name_1
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
                                [ (builtin unConstrData) __constr_name_1 ]
                              ]
                            ]
                          )
                          datum
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