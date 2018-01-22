namespace WebSharper.UI.Formlets

open System.Collections.Generic
open System.Runtime.CompilerServices
open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Notation

[<JavaScript>]
type LayoutShape =
    | Item of Doc
    | Varying of View<list<Layout>>
    | Horizontal of list<Layout>
    | Vertical of list<Layout>
    | Wrap of LayoutShape * (Doc -> Doc)

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

    static member Wrap f l =
        { l with Shape = LayoutShape.Wrap (l.Shape, f) }

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
        let rec hl x =
            x |> List.rev
            |> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> hl ls
                | Vertical ls -> [hel l (Doc.Concat (vl ls))]
                | Item x -> [hel l x]
                | Varying v -> [v |> Doc.BindView (hl >> Doc.Concat)]
                | Wrap (l, f) -> [f (full {Shape = l; Label = None})])
        and vl x =
            x |> List.rev
            |> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> [vel l (Doc.Concat (hl ls))]
                | Vertical ls -> vl ls
                | Item x -> [vel l x]
                | Varying v -> [v |> Doc.BindView (vl >> Doc.Concat)]
                | Wrap (l, f) -> [f (full {Shape = l; Label = None})])
        and full l = Doc.Concat (vl [l])
        full l

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
        let rec hl x =
            x |> List.rev
            |> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> hl ls
                | Vertical ls -> [hel l (vwrap (vl ls))]
                | Item x -> [hel l x]
                | Varying v -> [v |> Doc.BindView (hl >> Doc.Concat)]
                | Wrap (l, f) -> [f (full {Shape = l; Label = None})])
        and vl x =
            x |> List.rev
            |> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> [vel l (hwrap (hl ls))]
                | Vertical ls -> vl ls
                | Item x -> [vel l x]
                | Varying v -> [v |> Doc.BindView (vl >> Doc.Concat)]
                | Wrap (l, f) -> [f (full {Shape = l; Label = None})])
        and full l =
            match l.Shape with
            | Horizontal ls -> hwrap (hl ls)
            | Vertical ls -> vwrap (vl ls)
            | Item _ | Varying _ -> vwrap (vl [l])
            | Wrap (l, f) -> f (full {Shape = l; Label = None})
        full l

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

    let IsSuccess x =
        match x with
        | Success _ -> true
        | Failure _ -> false

    let IsFailure x =
        match x with
        | Success _ -> false
        | Failure _ -> true

type internal FormletData<'T> =
    {
        View : View<Result<'T>>
        Layout : list<Layout>
    }

[<JavaScript>]
type Formlet<'T> =
    internal | Formlet of (unit -> FormletData<'T>)

    member internal this.Data =
        let (Formlet d) = this
        d

[<JavaScript>]
module Formlet =

    let Return x =
        Formlet (fun () ->
            {
                View = View.Const (Success x)
                Layout = []
            })

    let RunWithLayout render f (Formlet flX) =
        let flX = flX ()
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
        Formlet (fun () ->
            let flX = flX ()
            { flX with Layout = f flX.Layout })

    let MapResult f (Formlet flX) =
        Formlet (fun () ->
            let flX = flX ()
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
        Formlet (fun () ->
            let flX = flX ()
            let sub = Submitter.Create flX.View (Failure [])
            {
                View = sub.View
                Layout =
                    Layout.Item (
                        Doc.Element "input" [
                            Attr.Create "type" "button"
                            Attr.Create "value" txt
                            Attr.Class "submitButton"
                            Attr.Handler "click" (fun _ _ -> sub.Trigger())
                            Attr.DynamicPred "disabled"
                                (sub.Input.Map Result.IsFailure)
                                (View.Const "disabled")
                        ] [])
                    :: flX.Layout
            })

    let Horizontal (Formlet flX) =
        Formlet (fun () ->
            let flX = flX ()
            let rec toHorizontal = function
                | [] -> []
                | [l] ->
                    [(match l.Shape with
                        | Vertical ls -> { l with Shape = Horizontal ls }
                        | Varying v -> { l with Shape = Varying (View.Map toHorizontal v) }
                        | _ -> l)]
                | ls -> [{ Shape = Horizontal ls; Label = None }]
            { flX with Layout = toHorizontal flX.Layout })

    let Vertical (Formlet flX) =
        Formlet (fun () ->
            let flX = flX ()
            let rec toVertical = function
                | [] -> []
                | [l] ->
                    [(match l.Shape with
                        | Horizontal ls -> { l with Shape = Vertical ls }
                        | Varying v -> { l with Shape = Varying (View.Map toVertical v) }
                        | _ -> l)]
                | ls -> [{ Shape = Vertical ls; Label = None }]
            { flX with Layout = toVertical flX.Layout })

    let Many (Formlet f) =
        Formlet (fun () ->
            let m = ListModel.Create fst []
            let v =
                m.View
                |> View.MapSeqCachedBy m.Key snd
                |> View.Map Array.ofSeq
            {
                View =
                    v.Bind(
                        Array.MapTreeReduce
                            (fun fl -> fl.View.Map (Result.Map Seq.singleton))
                            (View.Const (Result<_>.Success Seq.empty))
                            (View.Map2 Result.Append))
                Layout =
                    [
                        Layout.Item (
                            Doc.Element "div" [
                                Attr.Class "addIcon"
                                Attr.Handler "click" (fun _ _ -> m.Add (Key.Fresh(), f ()))
                            ] [])
                        Layout.Varying (
                            v.Map (
                                Array.MapTreeReduce
                                    (fun fl -> fl.Layout)
                                    List.empty
                                    (fun x y -> y @ x)))
                    ]
            })

    let ManyWithModel (m: ListModel<'K, 'T>) (insertInit: unit -> 'T) (f: Var<'T> -> Formlet<'U>) =
        Formlet (fun () ->
            let mf = ListModel.Create fst []
            m.Iter(fun x ->
                let k = m.Key x
                mf.Add(k, (f (m.Lens k)).Data ()))
            let cb =
                m.View.Map (fun xs ->
                    for x in xs do
                        let k = m.Key x
                        if not (mf.ContainsKey k) then
                            mf.Add(k, (f (m.Lens k)).Data ())
                    for (k, _) in mf.Value do
                        if not (m.ContainsKey k) then
                            mf.RemoveByKey k
                    Doc.Empty)
            let v = mf.View.Map (Seq.map snd >> Array.ofSeq)
            {
                View =
                    v.Bind(
                        Array.MapTreeReduce
                            (fun fl -> fl.View.Map (Result.Map Seq.singleton))
                            (View.Const (Result<_>.Success Seq.empty))
                            (View.Map2 Result.Append))
                Layout =
                    [
                        Layout.Item (
                            Doc.Element "div" [
                                Attr.Class "addIcon"
                                Attr.Handler "click" (fun _ _ -> m.Add (insertInit()))
                            ] []
                            |> Doc.Append (Doc.EmbedView cb))
                        Layout.Varying (
                            v.Map (
                                Array.MapTreeReduce
                                    (fun fl -> fl.Layout)
                                    List.empty
                                    (fun x y -> y @ x)))
                    ]
            })

    let Bind (Formlet flX : Formlet<'T>) (f: 'T -> Formlet<'U>) : Formlet<'U> =
        Formlet (fun () ->
            let flX = flX ()
            let v = flX.View.Map (Result.Map (fun x -> ((f x).Data ())))
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
        Formlet (fun () ->
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

    let WrapLayout f flX =
        flX |> MapLayout (fun ls -> [Layout.OfList ls |> Layout.Wrap f])

    let WithFormContainer flX =
        flX |> WrapLayout (fun d ->
            Doc.Element "form" [Attr.Class "formlet"] [d] :> _)

    type Builder internal () =
        member this.Return x = Return x
        member this.ReturnFrom flX = flX
        member this.Bind(flX, f) = Bind flX f

    let Do = Builder()

[<AutoOpen; JavaScript>]
module Pervasives =

    let (<*>) (Formlet flF) (Formlet flX) =
        Formlet (fun () ->
            let flF = flF ()
            let flX = flX ()
            {
                View = View.Map2 Result.Apply flF.View flX.View
                Layout = flX.Layout @ flF.Layout
            })

[<JavaScript>]
module Controls =

    let private Input' (var: unit -> Var<'T>) (layout: Var<'T> -> #Doc) : Formlet<'T> =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (layout var)]
            })
    let private InputVar' var layout =
        Input' (fun () -> var) layout
    let private InputInit' init layout =
        Input' (fun () -> Var.Create init :> _) layout

    // Input (string)
    let private InputLayout var = Doc.Input [Attr.Class "inputText"] var
    let InputVar var = InputVar' var InputLayout
    let Input init = InputInit' init InputLayout

    // IntInput (CheckedInput<int>)
    let private IntInputLayout var = Doc.IntInput [Attr.Class "inputText"] var
    let IntInputVar var = InputVar' var IntInputLayout
    let IntInput init = InputInit' init IntInputLayout

    // IntInputUnchecked (int)
    let private IntInputUncheckedLayout var = Doc.IntInputUnchecked [Attr.Class "inputText"] var
    let IntInputVarUnchecked var = InputVar' var IntInputUncheckedLayout
    let IntInputUnchecked init = InputInit' init IntInputUncheckedLayout

    // FloatInput (CheckedInput<Float>)
    let private FloatInputLayout var = Doc.FloatInput [Attr.Class "inputText"] var
    let FloatInputVar var = InputVar' var FloatInputLayout
    let FloatInput init = InputInit' init FloatInputLayout

    // FloatInputUnchecked (float)
    let private FloatInputUncheckedLayout var = Doc.FloatInputUnchecked [Attr.Class "inputText"] var
    let FloatInputVarUnchecked var = InputVar' var FloatInputUncheckedLayout
    let FloatInputUnchecked init = InputInit' init FloatInputUncheckedLayout

    // CheckBox (bool)
    let private CheckBoxLayout var = Doc.CheckBox [Attr.Class "inputCheckbox"] var
    let CheckBoxVar var = InputVar' var CheckBoxLayout
    let CheckBox init = InputInit' init CheckBoxLayout

    // Select ('T)
    let private Select' mk (items: list<'T * string>) =
        let labels = Dictionary()
        let values =
            items
            |> List.map (fun (value, label) ->
                labels.[value] <- label
                value)
        mk (Doc.Select [] (fun v -> labels.[v]) values)
    let SelectVar var items = Select' (InputVar' var) items
    let Select init items = Select' (InputInit' init) items

    // Radio ('T)
    let private Radio' (var: unit -> Var<'T>) (items: list<'T * string>) =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map Success
                Layout =
                    (items
                    |> List.rev
                    |> List.map (fun (value, label) ->
                        Layout.Item (
                            Doc.Element "label" [] [
                                Doc.Radio [Attr.Class "inputRadio"] value var
                                Doc.TextNode label
                            ])))
            })
    let RadioVar var items =
        Radio' (fun () -> var) items
    let Radio init items =
        Radio' (fun () -> Var.Create init :> _) items

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
