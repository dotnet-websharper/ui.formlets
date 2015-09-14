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
        let rec hl =
            List.rev
            >> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> hl ls
                | Vertical ls -> [hel l (Doc.Concat (vl ls))]
                | Item x -> [hel l x]
                | Varying v -> [v |> Doc.BindView (hl >> Doc.Concat)]
                | Wrap (l, f) -> [f (full {Shape = l; Label = None})])
        and vl =
            List.rev
            >> List.collect (fun l ->
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
        let rec hl =
            List.rev
            >> List.collect (fun l ->
                match l.Shape with
                | Horizontal ls -> hl ls
                | Vertical ls -> [hel l (vwrap (vl ls))]
                | Item x -> [hel l x]
                | Varying v -> [v |> Doc.BindView (hl >> Doc.Concat)]
                | Wrap (l, f) -> [f (full {Shape = l; Label = None})])
        and vl =
            List.rev
            >> List.collect (fun l ->
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
                        Layout.Item (
                            Doc.Element "div" [
                                Attr.Class "addIcon"
                                Attr.Handler "click" (fun _ _ -> m.Add (Key.Fresh(), f ()))
                            ] [])
                        Layout.Varying (
                            v.Map (
                                Array.MapReduce
                                    (fun fl -> fl.Layout)
                                    List.empty
                                    (fun x y -> y @ x)))
                    ]
            })

    let ManyWithModel (m: ListModel<'K, 'T>) (insertInit: unit -> 'T) (f: IRef<'T> -> Formlet<'U>) =
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
                        Array.MapReduce
                            (fun fl -> fl.View.Map (Result.Map Seq.singleton))
                            (View.Const (Result.Success Seq.empty))
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
                                Array.MapReduce
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

    // Input (string)
    let private Input' (var: unit -> IRef<string>) =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.Input [Attr.Class "inputText"] var)]
            })
    let InputVar var =
        Input' (fun () -> var)
    let Input init =
        Input' (fun () -> Var.Create init :> _)

    // IntInput (CheckedInput<int>)
    let private IntInput' (var: unit -> IRef<CheckedInput<int>>) onBlank onInvalid =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map (function
                        | CheckedInput.Valid (x, _) -> Success x
                        | CheckedInput.Blank _ -> onBlank
                        | CheckedInput.Invalid txt -> onInvalid txt)
                Layout = [Layout.Item (Doc.IntInput [Attr.Class "inputText"] var)]
            })
    let IntInputVar var onBlank onInvalid =
        IntInput' (fun () -> var) onBlank onInvalid
    let IntInput init onBlank onInvalid =
        IntInput' (fun () -> Var.Create (CheckedInput.Valid init) :> _) onBlank onInvalid

    // IntInputUnchecked (int)
    let private IntInputUnchecked' (var: unit -> IRef<int>) =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.IntInputUnchecked [Attr.Class "inputText"] var)]
            })
    let IntInputVarUnchecked var =
        IntInputUnchecked' (fun () -> var)
    let IntInputUnchecked init =
        IntInputUnchecked' (fun () -> Var.Create init :> _)

    // FloatInput (CheckedInput<Float>)
    let private FloatInput' (var: unit -> IRef<CheckedInput<float>>) onBlank onInvalid =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map (function
                        | CheckedInput.Valid (x, _) -> Success x
                        | CheckedInput.Blank _ -> onBlank
                        | CheckedInput.Invalid txt -> onInvalid txt)
                Layout = [Layout.Item (Doc.FloatInput [Attr.Class "inputText"] var)]
            })
    let FloatInputVar var onBlank onInvalid =
        FloatInput' (fun () -> var) onBlank onInvalid
    let FloatInput init onBlank onInvalid =
        FloatInput' (fun () -> Var.Create (CheckedInput.Valid init) :> _) onBlank onInvalid

    // FloatInputUnchecked (float)
    let private FloatInputUnchecked' (var: unit -> IRef<float>) =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.FloatInputUnchecked [Attr.Class "inputText"] var)]
            })
    let FloatInputVarUnchecked var =
        FloatInputUnchecked' (fun () -> var)
    let FloatInputUnchecked init =
        FloatInputUnchecked' (fun () -> Var.Create init :> _)

    // CheckBox (bool)
    let private CheckBox' (var: unit -> IRef<bool>) =
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.CheckBox [Attr.Class "inputCheckbox"] var)]
            })
    let CheckBoxVar var =
        CheckBox' (fun () -> var)
    let CheckBox init =
        CheckBox' (fun () -> Var.Create init :> _)

    // Select ('T)
    let private Select' (var: unit -> IRef<'T>) (items: list<'T * string>) =
        let labels = Dictionary()
        let values =
            items
            |> List.map (fun (value, label) ->
                labels.[value] <- label
                value)
        Formlet (fun () ->
            let var = var()
            {
                View = var.View |> View.Map Success
                Layout = [Layout.Item (Doc.Select [] (fun v -> labels.[v]) values var)]
            })
    let SelectVar var items =
        Select' (fun () -> var) items
    let Select init items =
        Select' (fun () -> Var.Create init :> _) items

    // Radio ('T)
    let private Radio' (var: unit -> IRef<'T>) (items: list<'T * string>) =
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
