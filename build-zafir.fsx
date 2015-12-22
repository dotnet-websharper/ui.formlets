#load "tools/includes.fsx"

open IntelliFactory.Build

let bt =
    BuildTool().PackageId("Zafir.UI.Next.Formlets")
        .VersionFrom("Zafir", "alpha")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun fw -> fw.Net40)

let main =
    bt.Zafir.Library("WebSharper.UI.Next.Formlets")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("Zafir.UI.Next").Latest(true).ForceFoundVersion().Reference()
            ])
        .Embed(
            [
                "styles/Formlet.css"
                "images/ActionAdd.png"
                "images/ActionCheck.png"
                "images/ActionDelete.png"
                "images/ErrorIcon.png"
                "images/InfoIcon.png"
            ])

let test =
    bt.Zafir.SiteletWebsite("WebSharper.UI.Next.Formlets.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("Zafir.UI.Next").Latest(true).ForceFoundVersion().Reference()
                r.Project(main)
            ])

bt.Solution [

    main
    test

    bt.NuGet.CreatePackage()
        .Description("Provides a framework to build reactive forms in Zafir.")
        .ProjectUrl("http://github.com/intellifactory/websharper.ui.next.formlets")
        .Configure(fun c ->
            {
                c with
                    Authors = ["IntelliFactory"]
                    Title = Some "Zafir.UI.Next.Formlets"
                    LicenseUrl = Some "http://github.com/intellifactory/websharper.ui.next.formlets/blob/master/LICENSE.md"
                    RequiresLicenseAcceptance = true
            })
        .Add(main)

]
|> bt.Dispatch
