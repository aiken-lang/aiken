(program
  1.1.0
  [
    (lam
      __constr_index_exposer
      [
        (lam
          __constr_fields_exposer
          [
            (lam
              value_id_0
              [
                [ (builtin equalsData) [ (builtin listData) value_id_0 ] ]
                [ (builtin listData) value_id_0 ]
              ]
            )
            [
              [
                (force (builtin mkCons))
                [
                  [
                    (lam as_data as_data)
                    (lam __no_inline__ (lam data_id_1 data_id_1))
                  ]
                  [
                    (builtin iData)
                    [
                      [
                        (lam
                          __cyclic_function_1
                          [
                            (lam
                              __cyclic_function_1
                              [
                                __cyclic_function_0
                                (lam reprog (lam reprof reprof))
                              ]
                            )
                            [ __cyclic_function_1 __cyclic_function_1 ]
                          ]
                        )
                        (lam
                          __no_inline__
                          (lam
                            __cyclic_function_1
                            (lam
                              __chooser
                              [
                                [
                                  __chooser
                                  (lam
                                    n_id_3
                                    (force
                                      [
                                        [
                                          [
                                            (force (builtin ifThenElse))
                                            [
                                              [
                                                (builtin lessThanEqualsInteger)
                                                n_id_3
                                              ]
                                              (con integer 0)
                                            ]
                                          ]
                                          (delay (con integer 0))
                                        ]
                                        (delay
                                          [
                                            [
                                              [
                                                __cyclic_function_1
                                                __cyclic_function_1
                                              ]
                                              (lam
                                                __no_inline__
                                                (lam reprog (lam reprof reprof))
                                              )
                                            ]
                                            [
                                              [
                                                (builtin addInteger)
                                                (con integer -1)
                                              ]
                                              n_id_3
                                            ]
                                          ]
                                        )
                                      ]
                                    )
                                  )
                                ]
                                (lam
                                  n_id_2
                                  (force
                                    [
                                      [
                                        [
                                          (force (builtin ifThenElse))
                                          [
                                            [
                                              (builtin lessThanEqualsInteger)
                                              n_id_2
                                            ]
                                            (con integer 0)
                                          ]
                                        ]
                                        (delay (con integer 0))
                                      ]
                                      (delay
                                        [
                                          [
                                            [
                                              __cyclic_function_1
                                              __cyclic_function_1
                                            ]
                                            (lam
                                              __no_inline__
                                              (lam reprog (lam reprof reprog))
                                            )
                                          ]
                                          [
                                            [
                                              (builtin addInteger)
                                              (con integer -1)
                                            ]
                                            n_id_2
                                          ]
                                        ]
                                      )
                                    ]
                                  )
                                )
                              ]
                            )
                          )
                        )
                      ]
                      (con integer 0)
                    ]
                  ]
                ]
              ]
              [
                [ (force (builtin mkCons)) [ (builtin iData) (con integer 0) ] ]
                (con (list data) [])
              ]
            ]
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
    )
    (lam
      __constr_var
      [
        (force (force (builtin fstPair)))
        [ (builtin unConstrData) __constr_var ]
      ]
    )
  ]
)
