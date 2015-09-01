namespace WebSharper.UI.Next.Formlets

open System.Collections.Generic
open System.Runtime.CompilerServices
open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Notation

[<JavaScript>]
type LayoutShape =
    | Item of Doc
    | Varying of View<list<Layout>>
    | Horizontal of list<Layout>
    | Vertical of list<Layout>

and [<JavaScript>] Layout =
    {
        Shape : LayoutShape
        Label : option<Doc>
    }

    static member Horizontal xs =
        {
            Shape = LayoutShape.Horizontal xs
            Label = None
        }

    static member Vertical xs =
        {
            Shape = LayoutShape.Vertical xs
            Label = None
        }

    static member Item doc =
        {
            Shape = LayoutShape.Item doc
            Label = None
        }

    static member Varying view =
        {
            Shape = LayoutShape.Varying view
            Label = None
        }

    static member OfList ls =
        match ls with
        | [l] -> l
        | ls -> Layout.Vertical ls

    static member Flat l =
        let label l (x: Doc) =
            match l.Label with
            | None -> [x]
            | Some l -> [Doc.Element "label" [] [l; x]]
        let hel l x =
            Doc.Element "div" [Attr.Style "display" "inline-block"] (label l x) :> Doc
        let vel l x =
            Doc.Element "div" [] (label l x) :> Doc
        let rec hl =
            List.rev
            >> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> hl ls
                | Vertical ls -> [hel l (Doc.Concat (vl ls))]
                | Item x -> [hel l x]
                | Varying v -> [v |> Doc.BindView (hl >> Doc.Concat)])
        and vl =
            List.rev
            >> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> [vel l (Doc.Concat (hl ls))]
                | Vertical ls -> vl ls
                | Item x -> [vel l x]
                | Varying v -> [v |> Doc.BindView (vl >> Doc.Concat)])
        Doc.Concat (vl [l])

    static member Table l =
        let vel l x =
            Doc.Element "tr" [] [
                Doc.Element "td" [] (Option.toList l.Label)
                Doc.Element "td" [] [x]
            ] :> Doc
        let vwrap vels =
            Doc.Element "table" [] [
                Doc.Element "tbody" [] vels
            ]
            :> Doc
        let hel l x =
            Doc.Append
                (match l.Label with
                    | None -> Doc.Empty
                    | Some l -> Doc.Element "td" [] [l] :> _)
                (Doc.Element "td" [] [x])
        let hwrap hels =
            vwrap [Doc.Element "tr" [] hels]
        let rec hl =
            List.rev
            >> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> hl ls
                | Vertical ls -> [hel l (vwrap (vl ls))]
                | Item x -> [hel l x]
                | Varying v -> [v |> Doc.BindView (hl >> Doc.Concat)])
        and vl =
            List.rev
            >> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> [vel l (hwrap (hl ls))]
                | Vertical ls -> vl ls
                | Item x -> [vel l x]
                | Varying v -> [v |> Doc.BindView (vl >> Doc.Concat)])
        match l.Shape with
        | Horizontal ls -> hwrap (hl ls)
        | Vertical ls -> vwrap (vl ls)
        | Item _ | Varying _ -> vwrap (vl [l])

type Result<'T> =
    | Success of 'T
    | Failure of list<string>

[<JavaScript>]
module Result =

    let Map f rX =
        match rX with
        | Success x -> Success (f x)
        | Failure m -> Failure m

    let Bind f rX =
        match rX with
        | Success x -> f x
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
        Layout : list<Layout>
    }

[<JavaScript>]
type Formlet<'T> =
    internal | Formlet of ((Layout -> Doc) -> FormletData<'T>)

    member internal this.Data =
        let (Formlet d) = this
        d

[<JavaScript>]
module Formlet =

    let Return x =
        Formlet (fun render ->
            {
                View = View.Const (Success x)
                Layout = []
            })

    let RunWithLayout render f (Formlet flX) =
        let flX = flX render
        flX.View
        |> Doc.BindView (fun r ->
            match r with
            | Success x -> f x
            | Failure _ -> ()
            Doc.Empty)
        |> Doc.Append (render (Layout.OfList flX.Layout))

    let Run f flX =
        RunWithLayout Layout.Flat f flX

    let MapLayout f (Formlet flX) =
        Formlet (fun render ->
            let flX = flX render
            { flX with Layout = f flX.Layout })

    let MapResult f (Formlet flX) =
        Formlet (fun render ->
            let flX = flX render
            {
                View = flX.View.Map f
                Layout = flX.Layout
            })

    let MapToResult f flX =
        MapResult (Result.Bind f) flX

    let Map f flX =
        MapResult (Result.Map f) flX

    let LiftResult flX =
        MapResult Success flX

    let WithSubmit txt (Formlet flX) =
        Formlet (fun render ->
            let flX = flX render
            let sub = Submitter.Create flX.View (Failure [])
            {
                View = sub.View
                Layout = Layout.Item (Doc.Button txt [] sub.Trigger) :: flX.Layout
            })

    let Horizontal (Formlet flX) =
        Formlet (fun render ->
            let flX = flX render
            { flX with
                Layout =
                    match flX.Layout with
                    | [{Shape = Vertical ls}] -> [Layout.Horizontal ls]
                    | [] | [_] as ls -> ls
                    | ls -> [Layout.Horizontal ls]
            })

    let Vertical (Formlet flX) =
        Formlet (fun render ->
            let flX = flX render
            { flX with
                Layout =
                    match flX.Layout with
                    | [{Shape = Horizontal ls}] -> [Layout.Vertical ls]
                    | [] | [_] as ls -> ls
                    | ls -> [Layout.Vertical ls]
            })

    let Many (Formlet f) =
        Formlet (fun render ->
            let m = ListModel.Create fst []
            let v =
                m.View
                |> View.ConvertBy m.Key snd
                |> View.Map Array.ofSeq
            {
                View =
                    v.Bind(
                        Array.MapReduce
                            (fun fl -> fl.View.Map (Result.Map Seq.singleton))
                            (View.Const (Result.Success Seq.empty))
                            (View.Map2 Result.Append))
                Layout =
                    [
                        Layout.Item (Doc.Button "Add" [] (fun () -> m.Add (Key.Fresh(), f render)))
                        Layout.Varying (
                            v.Map (
                                Array.MapReduce
                                    (fun fl -> fl.Layout)
                                    List.empty
                                    (fun x y -> y @ x)))
                    ]
            })

    let ManyWithModel (m: ListModel<'K, 'T>) (insertInit: unit -> 'T) (f: IRef<'T> -> Formlet<'U>) =
        Formlet (fun render ->
            let mf = ListModel.Create fst []
            m.Iter(fun x ->
                let k = m.Key x
                mf.Add(k, (f (m.Lens k)).Data render))
            let cb =
                m.View.Map (fun xs ->
                    for x in xs do
                        let k = m.Key x
                        if not (mf.ContainsKey k) then
                            mf.Add(k, (f (m.Lens k)).Data render)
                    for (k, _) in mf.Value do
                        if not (m.ContainsKey k) then
                            mf.RemoveByKey k
                    Doc.Empty)
            let v = mf.View.Map (Seq.map snd >> Array.ofSeq)
            {
                View =
                    v.Bind(
                        Array.MapReduce
                            (fun fl -> fl.View.Map (Result.Map Seq.singleton))
                            (View.Const (Result.Success Seq.empty))
                            (View.Map2 Result.Append))
                Layout =
                    [
                        Layout.Item (
                            Doc.Button "Add" [] (fun () -> m.Add (insertInit()))
                            |> Doc.Append (Doc.EmbedView cb))
                        Layout.Varying (
                            v.Map (
                                Array.MapReduce
                                    (fun fl -> fl.Layout)
                                    List.empty
                                    (fun x y -> y @ x)))
                    ]
            })

    let Bind (Formlet flX : Formlet<'T>) (f: 'T -> Formlet<'U>) : Formlet<'U> =
        Formlet (fun render ->
            let flX = flX render
            let v = flX.View.Map (Result.Map (fun x -> ((f x).Data render)))
            {
                View = v.Bind (function
                    | Success x -> x.View
                    | Failure m -> View.Const (Failure m))
                Layout =
                    Layout.Varying (
                        v.Map (function
                            | Failure _ -> []
                            | Success flY -> flY.Layout))
                    :: flX.Layout
            })

    let OfDoc (f: unit -> Doc) =
        Formlet (fun render ->
            {
                View = View.Const (Success ())
                Layout = [Layout.Item (f ())]
            })

    let WithLabel label flX =
        MapLayout (function
            | [x] -> [{x with Label = Some label}]
            | ls -> [{Shape = LayoutShape.Vertical ls; Label = Some label}])
            flX

    let WithTextLabel label flX =
        flX |> MapLayout (fun l ->
            let label = Some (Doc.Element "label" [] [Doc.TextNode label] :> Doc)
            match l with
            | [x] -> [{x with Label = label}]
            | ls -> [{Shape = LayoutShape.Vertical ls; Label = label}])

    type Builder internal () =
        member this.Return x = Return x
        member this.ReturnFrom flX = flX
        member this.Bind(flX, f) = Bind flX f

    let Do = Builder()

[<AutoOpen; JavaScript>]
module Pervasives =

    let (<*>) (Formlet flF) (Formlet flX) =
        Formlet (fun render ->
            let flF = flF render
            let flX = flX render
            {
                View = View.Map2 Result.Apply flF.View flX.View
                Layout = flX.Layout @ flF.Layout
            })

[<JavaScript>]
module Controls =

    let InputVar (var: IRef<string>) =
        Formlet (fun render ->
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.Input [] var)]
            })

    let Input init =
        Formlet (fun render ->
            let var = Var.Create init
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.Input [] var)]
            })

    let CheckBoxVar (var: IRef<bool>) =
        Formlet (fun render ->
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.CheckBox [] var)]
            })

    let CheckBox init =
        Formlet (fun render ->
            let var = Var.Create init
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.CheckBox [] var)]
            })

    let SelectVar (var: IRef<'T>) (items: list<'T * string>) =
        let labels = Dictionary()
        let values =
            items
            |> List.map (fun (value, label) ->
                labels.[value] <- label
                value)
        Formlet (fun render ->
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.Select [] (fun v -> labels.[v]) values var)]
            })

    let Select init items = SelectVar (Var.Create init) items

    let RadioVar (var: IRef<'T>) (items: list<'T * string>) =
        Formlet (fun render ->
            {
                View = var.View |> View.Map Success
                Layout =
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

[<JavaScript>]
module Validation =

    let Is pred msg flX =
        flX |> Formlet.MapToResult (fun x ->
            if pred x then Success x else Failure [msg])

    let IsNotEmpty msg flX =
        Is ((<>) "") msg flX

    let IsMatch regex msg flX =
        let re = RegExp(regex)
        Is re.Test msg flX
