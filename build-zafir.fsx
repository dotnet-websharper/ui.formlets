#load "tools/includes.fsx"

open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.UI.Formlets")
        .VersionFrom("WebSharper", "alpha")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun fw -> fw.Net40)

let main =
    bt.WebSharper4.Library("WebSharper.UI.Formlets")
        .SourcesFromProject()
        .WithSourceMap()
        .References(fun r ->
            [
                r.NuGet("WebSharper.UI").Latest(true).ForceFoundVersion().Reference()
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
    bt.WebSharper4.SiteletWebsite("WebSharper.UI.Formlets.Tests")
        .SourcesFromProject()
        .WithSourceMap()
        .References(fun r ->
            [
                r.NuGet("WebSharper.UI").Latest(true).ForceFoundVersion().Reference()
                r.Project(main)
            ])

bt.Solution [

    main
    test

    bt.NuGet.CreatePackage()
        .Description("Provides a framework to build reactive forms in WebSharper.")
        .ProjectUrl("http://github.com/dotnet-websharper/ui.formlets")
        .Configure(fun c ->
            {
                c with
                    Authors = ["IntelliFactory"]
                    Title = Some "WebSharper.UI.Formlets"
                    LicenseUrl = Some "http://github.com/dotnet-websharper/ui.formlets/blob/master/LICENSE.md"
                    RequiresLicenseAcceptance = true
            })
        .Add(main)

]
|> bt.Dispatch
