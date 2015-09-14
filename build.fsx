#load "tools/includes.fsx"

open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.UI.Next.Formlets")
        .VersionFrom("WebSharper", "alpha")
        .WithFramework(fun fw -> fw.Net40)

let main =
    bt.WebSharper.Library("WebSharper.UI.Next.Formlets")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("WebSharper.UI.Next").Reference()
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
    bt.WebSharper.SiteletWebsite("WebSharper.UI.Next.Formlets.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("WebSharper.UI.Next").Reference()
                r.Project(main)
            ])

bt.Solution [

    main
    test

    bt.NuGet.CreatePackage()
        .Description("Provides a framework to build reactive forms in WebSharper.")
        .ProjectUrl("http://github.com/intellifactory/websharper.ui.next.formlets")
        .Configure(fun c ->
            {
                c with
                    Authors = ["IntelliFactory"]
                    Title = Some "WebSharper.UI.Next.Formlets"
                    LicenseUrl = Some "http://github.com/intellifactory/websharper.ui.next.formlets/blob/master/LICENSE.md"
                    RequiresLicenseAcceptance = true
            })
        .Add(main)

]
|> bt.Dispatch
