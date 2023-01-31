(program
  1.0.0
  [
    (lam
      i_0
      [
        [
          (force
            (force
              (delay
                (delay
                  (lam
                    i_1
                    [
                      (force (delay (lam i_2 [ i_2 i_2 ])))
                      (lam
                        i_3
                        (lam
                          i_4
                          [
                            [
                              i_1 [ (force (delay (lam i_5 [ i_5 i_5 ]))) i_3 ]
                            ]
                            i_4
                          ]
                        )
                      )
                    ]
                  )
                )
              )
            )
          )
          (lam
            i_6
            (lam
              i_7
              [
                [
                  [
                    (force
                      (delay
                        (lam
                          i_8
                          (lam
                            i_9
                            (lam
                              i_10
                              [
                                [
                                  [ [ (force (builtin ifThenElse)) i_8 ] i_9 ]
                                  i_10
                                ]
                                (con unit ())
                              ]
                            )
                          )
                        )
                      )
                    )
                    [ [ (builtin lessThanEqualsInteger) i_7 ] (con integer 1) ]
                  ]
                  (lam i_11 i_7)
                ]
                (lam
                  i_12
                  [
                    [
                      (builtin addInteger)
                      [
                        i_6
                        [ [ (builtin subtractInteger) i_7 ] (con integer 1) ]
                      ]
                    ]
                    [
                      i_6 [ [ (builtin subtractInteger) i_7 ] (con integer 2) ]
                    ]
                  ]
                )
              ]
            )
          )
        ]
        i_0
      ]
    )
    (con integer 0)
  ]
)