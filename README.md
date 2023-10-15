# DISCONTINUED

**Compost is discontinued. Before starting to work with this library or thinking about forking it, please have a look at [Vide](https://github.com/vide-collabo/Vide), which is a successor of Compost.**

--------------------------

# compost

Compost is an F# interactive scripting library for sound generation and composition. It is part of my FsAdvent 2019 contribution. The blog post is located here (TODO).

## Setup

* Install paket: `dotnet tool install --global Paket`
* Run `paket install`
* Use VsCode, Visual Studio or Rider to execute the code in F# interactive

## Playing around

Have a look at the examples:

* src/0_localStateDemo.fsx 
* src/1_playSimpleSinus.fsx 
* src/MAIN_audioDemo.fsx

When you want to make your own experiments based on the core code (located in `src/lib`), you should add the following code to your own fsx file:

```fsharp
#load "./lib/playback.fsx"

open Core
open Blocks
open Playback
open Compose
open Notes

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// here you go...
```

## Platforms

Currently, I use the CsCore library that depends on .Net Framework and Windows, so Mac and Linux users have to work around using something else (please excuse).


**Have fun and Mery Christmas!**

