
module PlcChecker

open Absyn
open Environ

// The type checker can be seen as an interpreter that computes
// the type of an expression instead of its value.

let rec teval (e : expr) (env : plcType env) : plcType =
    // to be implemented 