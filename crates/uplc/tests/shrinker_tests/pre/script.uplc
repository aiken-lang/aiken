(program
  1.0.0
  (lam
    i_0
    (lam
      i_1
      (lam
        i_2
        (force
          [
            [
              [
                (force (builtin ifThenElse))
                [
                  (lam
                    i_3
                    [
                      (lam
                        i_4
                        [
                          (lam i_5 [ [ i_5 (con bool True) ] i_3 ])
                          (lam
                            i_6
                            (lam
                              i_7
                              (force
                                [
                                  [
                                    [ (force (builtin ifThenElse)) i_6 ]
                                    (delay i_7)
                                  ]
                                  (delay (con bool False))
                                ]
                              )
                            )
                          )
                        ]
                      )
                      (force
                        [
                          [
                            [ (force (builtin ifThenElse)) i_3 ]
                            (delay (con bool True))
                          ]
                          (delay (con bool False))
                        ]
                      )
                    ]
                  )
                  (con bool False)
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