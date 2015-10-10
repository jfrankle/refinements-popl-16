// main.fs
// Entry point for the synthesis system with negation.
module Synth.Main
open Util
open Lang
open System

[<EntryPoint>]
let main argv =
    // Do some patching.
    SampleJit.GenFn := GenR.GenT
    SampleAot.GenFn := GenR.GenT

    // Usage string.
    let usage =
        "usage:\n" +
        "Every flag must be set or it will go to its default value (marked with *)\n" +
        "--input         {filename}\n" +
        "--output        {*stdout | filename}\n" +
        //"--mode          {*synthesize | enumerate [steps] | sample [function] [steps]}\n" +
        //"--inhabit_check {*no | yes}\n" +
        "--verbose       {*no | yes}\n" +
        "--sampling      {*jit | aot [steps]}\n" +
        "--time          {*no | yes | data}\n" +
        "--enum_strategy {*raw | union_at_base | union_everywhere | union_everywhere_unsound}\n"

    // Retrieve the input file.
    let inputfile =
        if not(Array.exists ((=) "--input") argv) then None
        else Some argv.[Array.findIndex ((=) "--input") argv + 1]

    // Retrieve the output file if it exists.
    let outputfile = 
        if not(Array.exists ((=) "--output") argv) then None
        else Some (argv.[Array.findIndex ((=) "--output") argv + 1])

    // Retrieve the mode if it exists.
    let mode = 
        if not(Array.exists ((=) "--mode") argv) then Run.RSynthesize
        else
            let n = Array.findIndex ((=) "--mode") argv
            let mode_str = argv.[n + 1]
            if   mode_str = "enumerate" then Run.REnumerate(System.Int32.Parse argv.[n+2])
            elif mode_str = "sample"    then Run.RSample(argv.[n+2], System.Int32.Parse argv.[n+3])
            else                             Run.RSynthesize

    // Retrieve the enumeration strategy if it exists.
    let enum_strategy = 
        if not(Array.exists ((=) "--enum_strategy") argv) then EnumRaw
        else
            let n = Array.findIndex ((=) "--enum_strategy") argv
            let mode_str = argv.[n + 1]
            if   mode_str = "union_at_base"            then EnumUnionBase
            elif mode_str = "union_everywhere"         then EnumUnionEverywhere
            elif mode_str = "union_everywhere_unsound" then EnumUnionEverywhereUnsound
            elif mode_str = "raw"                      then EnumRaw
            else                                            failwith "Bad enum flag"

    // Retrieve the verbosity if it exists.
    let verbose = 
        if not(Array.exists ((=) "--verbose") argv) then false
        else argv.[Array.findIndex ((=) "--verbose") argv + 1] = "yes"

    // Retrieve the time if it exists.
    let print_time = 
        if not(Array.exists ((=) "--time") argv) then Run.TimeNo
        elif argv.[Array.findIndex ((=) "--time") argv + 1] = "yes"  then Run.TimeYes
        elif argv.[Array.findIndex ((=) "--time") argv + 1] = "data" then Run.TimeData
        else Run.TimeNo

    // Retrieve the inhabit check if it exists.
    let inhabit_check = 
        if not(Array.exists ((=) "--inhabit_check") argv) then false
        else argv.[Array.findIndex ((=) "--inhabit_check") argv + 1] = "yes"

    // Retrieve the sampling mode if it exists.
    let sampling =
        if not(Array.exists ((=) "--sampling") argv) then Run.SJit
        else
            let n = Array.findIndex ((=) "--sampling") argv
            if argv.[n+1] = "aot" then Run.SAot(System.Int32.Parse argv.[n+2])
            else Run.SJit

    // Go!
    if inputfile.IsNone then printfn "%s" usage
    else
        Run.Run(inputfile.Value, outputfile, mode, verbose, sampling, enum_strategy, inhabit_check, print_time)

    0 // exit code
