namespace WebSharper.UI.Formlets.Resources

open WebSharper

type Css() =
    inherit Resources.BaseResource("WebSharper.UI.Formlets.styles.Formlet.css")

[<assembly: WebResource("WebSharper.UI.Formlets.styles.Formlet.css", "text/css")>]
[<assembly: WebResource("WebSharper.UI.Formlets.images.ActionAdd.png", "image/png")>]
[<assembly: WebResource("WebSharper.UI.Formlets.images.ActionCheck.png", "image/png")>]
[<assembly: WebResource("WebSharper.UI.Formlets.images.ActionDelete.png", "image/png")>]
[<assembly: WebResource("WebSharper.UI.Formlets.images.ErrorIcon.png", "image/png")>]
[<assembly: WebResource("WebSharper.UI.Formlets.images.InfoIcon.png", "image/png")>]
[<assembly: Require(typeof<Css>)>]
do ()
