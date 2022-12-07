(program
  1.0.0
  (lam
    datum
    (lam
      rdmr
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
                              [
                                (builtin equalsByteString)
                                [
                                  (builtin unBData)
                                  [
                                    [
                                      __constr_get_field
                                      [ __constr_fields_exposer datum ]
                                    ]
                                    (con integer 0)
                                  ]
                                ]
                              ]
                              [
                                (builtin unBData)
                                [
                                  [
                                    __constr_get_field
                                    [ __constr_fields_exposer rdmr ]
                                  ]
                                  (con integer 0)
                                ]
                              ]
                            ]
                          )
                          [
                            [
                              (builtin mkCons)
                              [ (builtin bData) (con bytestring #f4) ]
                            ]
                            [
                              [ (builtin mkCons) rdmr ]
                              [ [ (builtin mkCons) datum ] (con listdata []) ]
                            ]
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