// Build.fsx using FAKE Domain-specific language for generating
// executables of class projects.

#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.MSBuild
nuget Fake.IO.Filesystem //"
#load "./.fake/build.fsx/intellisense.fsx"

open System
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

// *** Define Targets ***
Target.create "Clean" (fun _ ->
  Trace.log " --- Cleaning stuff --- "
  !! "**/*.fs"
  |> Seq.map (fun inp -> Fake.Core.String.replace ".fs" "" inp)
  |> Seq.iter (fun exe -> Shell.rm exe)

  !! "**/FSharp.Core.dll"
  |> Seq.iter (fun lib -> Shell.rm lib)
)

Target.create "Build" (fun _ ->
  Trace.log " --- Building the app --- "
  !! "**/*.fs"
  |> Seq.iter (fun input ->
    let output = Fake.Core.String.replace ".fs" "" input
    Shell.AsyncExec("fsharpc", "--nologo --target:exe -o:" + output + " " + input)
    // Todo: further research on FSharp Async
    |> Async.RunSynchronously
    |> ignore
    printfn "AppBuild-Output: %s -> %s" input output


  )
)

// *** Define Dependencies ***
"Clean"
  ==> "Build"

// *** Start Build ***
Target.runOrDefault "Build"
