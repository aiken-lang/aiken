use crate::ast::{Name, Program};

pub mod interner;
pub mod shrinker;
pub mod shrinker2;

pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    println!("PROG IS {}", program.to_pretty());
    let mut prog = program.run_once_pass();
    println!("PROG IS {}", prog.to_pretty());
    let mut prev_count = 0;

    loop {
        let (current_program, context) = prog.multi_pass();

        if context.node_count == prev_count {
            prog = current_program;
            break;
        } else {
            prog = current_program;
            prev_count = context.node_count;
        }
    }
    println!("PROG IS {}", prog.to_pretty());

    prog = prog
        .builtin_curry_reducer()
        .multi_pass()
        .0
        .builtin_curry_reducer();

    println!("PROG IS {}", prog.to_pretty());

    loop {
        let (current_program, context) = prog.multi_pass();

        if context.node_count == prev_count {
            prog = current_program;
            break;
        } else {
            prog = current_program;
            prev_count = context.node_count;
        }
    }

    println!("PROG IS {}", prog.to_pretty());

    let x = prog.clean_up();

    println!("PROG IS {}", x.to_pretty());

    x

    // prog.lambda_reducer_1()
    //     .inline_reducer_1()
    //     .identity_reducer_1()
    //     .lambda_reducer_1()
    //     .inline_reducer_1()
    //     .force_delay_reducer_1()
    //     .cast_data_reducer_1()
    //     .builtin_eval_reducer_2()
    //     .convert_arithmetic_ops_2()
    //     .builtin_curry_reducer_1()
    //     .lambda_reducer_1()
    //     .inline_reducer_1()
    //     .identity_reducer_1()
    //     .builtin_curry_reducer_1()
    //     .lambda_reducer_1()
    //     .inline_reducer_1()
    //     .remove_no_inlines_2()
}
