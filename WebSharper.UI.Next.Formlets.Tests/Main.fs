namespace WebSharper.Piglets.Next.Tests

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Formlets

[<JavaScript>]
module Main =

    type Country = HU | FR

    let Pair flX flY =
        Formlet.Return (fun x y -> (x, y))
        <*> flX
        <*> flY

    let Main =
        Console.Log("Running JavaScript Entry Point..")
        let va = Var.Create "a"
        let vb = Var.Create 2
        let b =
            Controls.RadioVar vb [
                0, "Terrible"
                1, "Not good"
                2, "Average"
                3, "Good"
                4, "Excellent"
            ]
        let lm =
            ListModel.Create (fst >> fst) [
                (Key.Fresh(), "b"), (true, HU)
                (Key.Fresh(), "d"), (false, FR)
            ]
        let manyModel =
            Formlet.ManyWithModel lm (fun () -> (Key.Fresh(), "b"), (true, HU)) (fun item ->
                Formlet.Return (fun b (c, d) -> b, c, d)
                <*> Controls.InputVar (item.Lens (fst >> snd) (fun ((a, b), cd) b' -> (a, b'), cd))
                <*> (Formlet.Return (fun x y -> x, y)
                    <*> Controls.CheckBoxVar (item.Lens (snd >> fst) (fun (ab, (c, d)) c' -> ab, (c', d)))
                    <*> Controls.SelectVar (item.Lens (snd >> snd) (fun (ab, (c, d)) d' -> ab, (c, d')))
                        [HU, "Hungary"; FR, "France"]
                    |> Formlet.Horizontal)
            )
        let f1 =
            Formlet.Return (fun (a, b) (c, d) -> a, b, c, d)
            <*> (Formlet.Return (fun x y -> x, y)
                <*> Controls.Input "a"
                <*> Controls.Input "b"
                |> Formlet.Horizontal)
            <*> (Formlet.Return (fun x y -> x, y)
                <*> Controls.CheckBox true
                <*> Controls.Select "HU" ["HU", "Hungary"; "FR", "France"]
                |> Formlet.Horizontal)
        Doc.Concat [
            buttonAttr [
                on.click (fun _ _ -> vb.Value <- int (Math.Random() * 5.))
            ] [text "Randomize b"]
//            Formlet.Many f1
            manyModel
            |> Formlet.WithSubmit "Submit"
            |> Formlet.Run (fun x -> Console.Log x; JS.Window?foo <- x)

        ]
        |> Doc.RunById "main"
