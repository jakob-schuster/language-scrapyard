mod core;
mod surface;
mod util;

use std::rc::Rc;

fn main() -> anyhow::Result<()> {
    let tm = surface::Tm {
        location: util::Location { start: 0, end: 0 },
        data: surface::TmData::Match {
            tm: Rc::new(surface::Tm {
                location: util::Location { start: 0, end: 0 },
                data: surface::TmData::IntLit { i: 1 },
            }),
            branches: vec![
                surface::Branch {
                    pattern: surface::Pattern {
                        location: util::Location { start: 0, end: 0 },
                        data: surface::PatternData::IntLit { i: 0 },
                    },
                    tm: surface::Tm {
                        location: util::Location { start: 0, end: 0 },
                        data: surface::TmData::BoolLit { b: true },
                    },
                },
                surface::Branch {
                    pattern: surface::Pattern {
                        location: util::Location { start: 0, end: 0 },
                        data: surface::PatternData::IntLit { i: 1 },
                    },
                    tm: surface::Tm {
                        location: util::Location { start: 0, end: 0 },
                        data: surface::TmData::IntLit { i: 10 },
                    },
                },
            ],
        },
    };

    let (ctm, cty) = surface::elab_infer(&surface::Context::default(), &tm).unwrap();
    let vtm = core::eval(&core::Env::default(), &ctm);

    println!("{}", vtm);
    Ok(())
}
