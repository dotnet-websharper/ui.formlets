namespace WebSharper.UI.Next.Formlets

open System.Collections.Generic
open System.Runtime.CompilerServices
open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Notation

[<AutoOpen; JavaScript>]
module private Utils =

    module Array =

        let MapReduce (f: 'A -> 'B) (z: 'B) (re: 'B -> 'B -> 'B) (a: 'A[]) : 'B =
            let rec loop off len =
                match len with
                | n when n <= 0 -> z
                | 1 when off >= 0 && off < a.Length ->
                    f a.[off]
                | n ->
                    let l2 = len / 2
                    let a = loop off l2
                    let b = loop (off + l2) (len - l2)
                    re a b
            loop 0 a.Length


[<JavaScript>]
type Layout =
    | Item of Doc
    | Varying of View<list<Layout>>
    | Horizontal of list<Layout>
    | Vertical of list<Layout>

    static member OfList ls =
        match ls with
        | [l] -> l
        | ls -> Layout.Vertical ls

    static member Render l =
        match l with
        | Horizontal ls ->
            List.fold (fun ds l ->
                Doc.Element "div" [Attr.Style "display" "inline-block"] [Layout.Render l] :> Doc
                :: ds)
                [] ls
            |> Doc.Concat
        | Vertical ls ->
            List.fold (fun ds l ->
                Doc.Element "div" [] [Layout.Render l] :> Doc
                :: ds)
                [] ls
            |> Doc.Concat
        | Item d -> d
        | Varying v ->
            v |> Doc.BindView (Layout.OfList >> Layout.Render)

type Result<'T> =
    | Success of 'T
    | Failure of list<string>

[<JavaScript>]
module Result =

    let Map f rX =
        match rX with
        | Success x -> Success (f x)
        | Failure m -> Failure m

    let Apply rF rX =
        match rF with
        | Success f ->
            match rX with
            | Success x -> Success (f x)
            | Failure mX -> Failure mX
        | Failure mF ->
            match rX with
            | Success _ -> Failure mF
            | Failure mX -> Failure (mF @ mX)

    let Append r1 r2 =
        match r1 with
        | Success s1 ->
            match r2 with
            | Success s2 -> Success (Seq.append s1 s2)
            | Failure _ -> r2
        | Failure m1 ->
            match r2 with
            | Success _ -> r2
            | Failure m2 -> Failure (m1 @ m2)

type internal FormletData<'T> =
    {
        View : View<Result<'T>>
        Layout : (Layout -> Doc) -> list<Layout>
    }

type Formlet<'T> = internal Formlet of (unit -> FormletData<'T>)

[<JavaScript>]
module Formlet =

    let Return x =
        Formlet (fun () ->
            {
                View = View.Const (Success x)
                Layout = fun _ -> []
            })

    let RunWithLayout layout f (Formlet flX) =
        let flX = flX()
        flX.View
        |> Doc.BindView (fun r ->
            match r with
            | Success x -> f x
            | Failure _ -> ()
            Doc.Empty)
        |> Doc.Append (layout (Layout.OfList (flX.Layout layout)))

    let Run f flX =
        RunWithLayout Layout.Render f flX

    let MapLayout f (Formlet flX) =
        Formlet (fun () ->
            let flX = flX()
            { flX with Layout = f flX.Layout })

    let WithSubmit txt (Formlet flX) =
        Formlet (fun () ->
            let flX = flX()
            let sub = Submitter.Create flX.View (Failure [])
            {
                View = sub.View
                Layout = fun render -> Layout.Item (Doc.Button txt [] sub.Trigger) :: flX.Layout render
            })

    let Horizontal (Formlet flX) =
        Formlet (fun () ->
            let flX = flX()
            { flX with
                Layout = fun render ->
                    match flX.Layout render with
                    | [Layout.Vertical ls] -> [Layout.Horizontal ls]
                    | [] | [_] as ls -> ls
                    | ls -> [Layout.Horizontal ls]
            })

    let Vertical (Formlet flX) =
        Formlet (fun () ->
            let flX = flX()
            { flX with
                Layout = fun render ->
                    match flX.Layout render with
                    | [Layout.Horizontal ls] -> [Layout.Vertical ls]
                    | [] | [_] as ls -> ls
                    | ls -> [Layout.Vertical ls]
            })

    let Many (Formlet f) =
        Formlet (fun () ->
            let m = ListModel.Create fst []
            let v =
                m.View
                |> View.ConvertSeqBy m.Key (fun k v -> v.Map snd)
                |> View.Map Array.ofSeq
            {
                View =
                    v.Bind(
                        Array.MapReduce
                            (fun vfl -> vfl.Bind(fun fl -> fl.View.Map(Result.Map Seq.singleton)))
                            (View.Const (Result.Success Seq.empty))
                            (View.Map2 Result.Append))
                Layout = fun render ->
                    [
                        Layout.Item (Doc.Button "Add" [] (fun () -> m.Add(Key.Fresh(), f())))
                        Layout.Varying (
                                v.Bind(
                                    Array.MapReduce
                                        (fun vfl -> vfl.Map(fun fl -> fl.Layout render))
                                        (View.Const List.empty)
                                        (View.Map2 List.append)))
                    ]
            })

    let ManyWithModel (m: ListModel<'K, 'T>) (insertInit: unit -> 'T) (f: IRef<'T> -> Formlet<'U>) =
        Formlet (fun () ->
            let v =
                m.View
                |> View.ConvertSeqBy m.Key (fun k v -> let (Formlet fl) = f (m.Lens k) in fl())
                |> View.Map Array.ofSeq
            {
                View =
                    v.Bind(
                        Array.MapReduce
                            (fun fl -> fl.View.Map(Result.Map Seq.singleton))
                            (View.Const (Result.Success Seq.empty))
                            (View.Map2 Result.Append))
                Layout = fun render ->
                    [
                        Layout.Item (Doc.Button "Add" [] (fun () -> m.Add(insertInit())))
                        Layout.Varying (
                            v.Map (
                                Array.rev
                                >> Array.toList
                                >> List.collect (fun fl -> fl.Layout render)))
                    ]
            })

    let OfDoc (f: unit -> Doc) =
        Formlet (fun () ->
            {
                View = View.Const (Success ())
                Layout = fun render -> [Item (f())]
            })

[<AutoOpen; JavaScript>]
module Pervasives =

    let (<*>) (Formlet flF) (Formlet flX) =
        Formlet (fun () ->
            let flF = flF()
            let flX = flX()
            {
                View = View.Map2 Result.Apply flF.View flX.View
                Layout = fun render -> flX.Layout render @ flF.Layout render
            })

[<JavaScript>]
module Controls =

    let InputVar (var: IRef<string>) =
        Formlet (fun () ->
            {
                View = var.View |> View.Map Success
                Layout = fun render -> [Layout.Item (Doc.Input [] var)]
            })

    let Input init =
        Formlet (fun () ->
            let var = Var.Create init
            {
                View = var.View |> View.Map Success
                Layout = fun render -> [Layout.Item (Doc.Input [] var)]
            })

    let CheckBoxVar (var: IRef<bool>) =
        Formlet (fun () ->
            {
                View = var.View |> View.Map Success
                Layout = fun render -> [Layout.Item (Doc.CheckBox [] var)]
            })

    let CheckBox init =
        Formlet (fun () ->
            let var = Var.Create init
            {
                View = var.View |> View.Map Success
                Layout = fun render -> [Layout.Item (Doc.CheckBox [] var)]
            })

    let SelectVar (var: IRef<'T>) (items: list<'T * string>) =
        let labels = Dictionary()
        let values =
            items
            |> List.map (fun (value, label) ->
                labels.[value] <- label
                value)
        Formlet (fun () ->
            {
                View = var.View |> View.Map Success
                Layout = fun render -> [Layout.Item (Doc.Select [] (fun v -> labels.[v]) values var)]
            })

    let Select init items = SelectVar (Var.Create init) items

    let RadioVar (var: IRef<'T>) (items: list<'T * string>) =
        Formlet (fun () ->
            {
                View = var.View |> View.Map Success
                Layout = fun render ->
                    (items
                    |> List.rev
                    |> List.map (fun (value, label) ->
                        Layout.Item (
                            Doc.Element "label" [] [
                                Doc.Radio [] value var
                                Doc.TextNode label
                            ])))
            })

    let Radio init items = RadioVar (Var.Create init) items
