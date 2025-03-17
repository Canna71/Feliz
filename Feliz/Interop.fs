module [<RequireQualifiedAccess>] Feliz.Interop

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Feliz.ReactApi


let reactApi : IReactApi = importDefault "react"
#if FABLE_COMPILER_3 || FABLE_COMPILER_4
let inline reactElement (name: string) (props: 'a) : ReactElement = import "createElement" "react"
[<Emit "createElement.apply(null, [$0, $1, ...$2])">]
let inline reactElementApply (name: string) (props: 'a) (nested: #seq<ReactElement>) : ReactElement = jsNative

#else
let reactElement (name: string) (props: 'a) : ReactElement = import "createElement" "react"
[<Emit "createElement.apply(null, [$0, $1, ...$2])">]
let inline reactElementApply (name: string) (props: 'a) (nested: #seq<ReactElement>) : ReactElement = jsNative

#endif
let inline mkAttr (key: string) (value: obj) : IReactProperty = unbox (key, value)
[<Emit "undefined">]
let undefined : obj = jsNative
let inline mkStyle (key: string) (value: obj) : IStyleAttribute = unbox (key, value)
let inline svgAttribute (key: string) (value: obj) : ISvgAttribute = unbox (key, value)
let inline reactElementWithChild (name: string) (child: 'a) =
    // printfn "reactElementWithChild: %A" child
    reactElement name (createObj [ "children" ==> ResizeArray [| child |] ])
let inline reactElementWithChildren (name: string) (children: #seq<ReactElement>) =
    // printfn "reactElementWithChildren: %A" children
    reactElement name  (createObj [ "children" ==> reactApi.Children.toArray (Array.ofSeq children) ])

[<Emit "console.log($0,$1)">]
let inline log (x: obj) : unit = jsNative
let inline createElement name (properties: IReactProperty list) : ReactElement =
    // printfn "createElement: %A" properties
    let obj : (string * obj) list = !!properties
    let nested = obj |> List.tryPick (function ("nested", v) -> Some v | _ -> None) |> Option.defaultValue []
    let others = obj |> List.filter (fun (k, v) -> k <> "nested")
    let props = createObj others
    reactElementApply name props !!nested
let inline createSvgElement name (properties: ISvgAttribute list) : ReactElement =
    reactElement name (createObj !!properties)

[<Emit "typeof $0 === 'number'">]
let isTypeofNumber (x: obj) : bool = jsNative
