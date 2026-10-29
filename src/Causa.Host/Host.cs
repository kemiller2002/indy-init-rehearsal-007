using System.Runtime.InteropServices.JavaScript;

namespace Causa;

[System.Runtime.Versioning.SupportedOSPlatform("browser")]
public static partial class Host
{
    [JSExport]
    public static string Dispatch(string message) => Causa.App.Engine.dispatch(message);

    public static void Main() { }
}
