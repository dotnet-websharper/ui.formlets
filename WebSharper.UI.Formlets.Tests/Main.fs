namespace WebSharper.UI.Formlets.Tests

open WebSharper
open WebSharper.UI
open WebSharper.UI.Html

[<JavaScript>]
module Client =
    open WebSharper.JavaScript
    open WebSharper.UI.Client
    open WebSharper.UI.Formlets

    type Country =
        | [<Constant "HU">] HU
        | [<Constant "FR">] FR

        static member ToString x =
            match x with HU -> "HU" | FR -> "FR"

    let Pair flX flY =
        Formlet.Return (fun x y -> (x, y))
        <*> flX
        <*> flY

    [<SPAEntryPoint>]
    let Main() =
        Console.Log("Running JavaScript Entry Point..")
        let lm =
            ListModel.Create (fst >> fst) [
                (Key.Fresh(), "b"), (true, HU)
                (Key.Fresh(), "d"), (false, FR)
            ]
        let manyModel =
            Formlet.ManyWithModel lm (fun () -> (Key.Fresh(), "b"), (true, HU)) (fun item ->
                Formlet.Return (fun b (c, d) -> b, c, d)
                <*> (Controls.InputVar (item.Lens (fst >> snd) (fun ((a, b), cd) b' -> (a, b'), cd))
                    |> Validation.IsNotEmpty "Please enter a first field value"
                    |> Formlet.WithTextLabel "First field:")
                <*> (Formlet.Do {
                        let! x = Controls.CheckBoxVar (item.Lens (snd >> fst) (fun (ab, (c, d)) c' -> ab, (c', d)))
                        let! y =
                            (if x then Controls.SelectVar else Controls.RadioVar)
                                (item.Lens (snd >> snd) (fun (ab, (c, d)) d' -> ab, (c, d')))
                                [HU, "Hungary"; FR, "France"]
                            |> Formlet.Horizontal
                        return x, y
                    }
                    |> Formlet.Horizontal
                    |> Formlet.WithTextLabel "Second field:"))
        let f1 =
            Formlet.Return (fun (a, b) (c, d) -> a, b, c, d)
            <*> (Formlet.Return (fun x y -> x, y)
                <*> Controls.Input "a"
                <*> Controls.Input "b"
                |> Formlet.Horizontal)
            <*> (Formlet.Return (fun x y -> x, y)
                <*> Controls.CheckBox true
                <*> Controls.Select HU [HU, "Hungary"; FR, "France"]
                |> Formlet.Horizontal)
//        Formlet.Many f1
//        |> Formlet.WithSubmit "Submit"
        Formlet.Do {
            let! res =
                manyModel
                |> Formlet.WithSubmit "Submit"
            return! Formlet.OfDoc (fun () ->
                res
                |> Seq.map (fun (b, c, d) ->
                    Console.Log d
                    p [] [text (sprintf "%s %b %s" b c (Country.ToString d))]
                    :> Doc)
                |> Doc.Concat)
        }
        |> Formlet.WithFormContainer
        |> Formlet.RunWithLayout Layout.Table (fun x ->
            JS.Window?foo <- x)
        |> Doc.RunById "main"
