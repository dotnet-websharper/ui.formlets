namespace WebSharper.UI.Next.Formlets.Resources

open WebSharper

type Css() =
    inherit Resources.BaseResource("Formlet.css")

[<assembly: System.Web.UI.WebResource("Formlet.css", "text/css")>]
[<assembly: System.Web.UI.WebResource("ActionAdd.png", "image/png")>]
[<assembly: System.Web.UI.WebResource("ActionCheck.png", "image/png")>]
[<assembly: System.Web.UI.WebResource("ActionDelete.png", "image/png")>]
[<assembly: System.Web.UI.WebResource("ErrorIcon.png", "image/png")>]
[<assembly: System.Web.UI.WebResource("InfoIcon.png", "image/png")>]
[<assembly: Require(typeof<Css>)>]
do ()
