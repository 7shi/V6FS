#r "System"
#r "System.Core"
#r "System.Drawing"
#r "System.Windows.Forms"

#load "V6FS/Utils.fs"
#load "V6FS/Crc.fs"
#load "V6FS/Deflate.fs"
#load "V6FS/Zip.fs"
#load "V6FS/V6Type.fs"
#load "V6FS/V6FS.fs"

open System
open System.IO
open System.Drawing
open System.Windows.Forms

[<STAThread>] do
Application.EnableVisualStyles()
Application.SetCompatibleTextRenderingDefault(false)

let f = new Form(Text = "V6FS")

let textBox1 = new TextBox(Dock = DockStyle.Fill,
                           Font = new Font(FontFamily.GenericMonospace, f.Font.Size),
                           Multiline = true,
                           ScrollBars = ScrollBars.Both,
                           WordWrap = false)
f.Controls.Add textBox1

let miFile        = new MenuItem(Text = "ファイル(&F)")
let miFileOpen    = new MenuItem(Text = "開く(&O)")
let miFileSaveZip = new MenuItem(Text = "&Zipで保存", Enabled = false)
let miFileSep1    = new MenuItem(Text = "-")
let miFileExit    = new MenuItem(Text = "終了(&X)")
miFile.MenuItems.AddRange [| miFileOpen; miFileSaveZip; miFileSep1; miFileExit |]

let mainMenu1 = new MainMenu()
mainMenu1.MenuItems.Add miFile |> ignore
f.Menu <- mainMenu1

let openFileDialog1 = new OpenFileDialog()
let saveFileDialog1 = new SaveFileDialog(Filter = "Zip ファイル (*.zip)|*.zip|すべてのファイル (*.*)|*.*")

let root = ref Unchecked.defaultof<V6FS.Entry>

let dialog (dlg: FileDialog) success _ =
    if dlg.ShowDialog(f) <> DialogResult.OK then () else
    let cur = Cursor.Current
    Cursor.Current <- Cursors.WaitCursor
    success dlg.FileName
    Cursor.Current <- cur

miFileOpen.Click.Add << dialog openFileDialog1 <| fun fn ->
    use sw = new StringWriter()
    try
        use fs = new FileStream(fn, FileMode.Open)
        root := V6FS.Open(fs)
        (!root).FileSystem.Write(sw)
        let rec dir (e: V6FS.Entry) =
            sw.WriteLine()
            e.Write(sw)
            for child in e.Children do
                dir(child)
        dir(!root)
        miFileSaveZip.Enabled <- true
        let zip = Path.ChangeExtension(fn, ".zip")
        saveFileDialog1.FileName <- Path.GetFileName(zip)
    with e ->
        sw.WriteLine(e.ToString())
        miFileSaveZip.Enabled <- false
        root := Unchecked.defaultof<V6FS.Entry>
    textBox1.Text <- sw.ToString()

miFileSaveZip.Click.Add << dialog saveFileDialog1 <| fun fn ->
    try
        use fs = new FileStream(fn, FileMode.Create)
        V6FS.SaveZip(fs, !root)
    with e ->
        ignore <| MessageBox.Show(
            e.ToString(), f.Text,
            MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

miFileExit.Click.Add <| fun _ -> f.Close()

Application.Run(f)
