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
              [ (force (builtin ifThenElse)) (con bool True) ]
              (delay (con unit ()))
            ]
            (delay (error ))
          ]
        )
      )
    )
  )
)