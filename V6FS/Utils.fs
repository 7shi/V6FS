// public domain

module Utils

open System
open System.IO
open System.Text

let right(s:string, len:int) = s.Substring(s.Length - len)

let getBinaryReader(data:byte[], offset:int) =
    new BinaryReader(new MemoryStream(data, offset, 512))

let getUInt32(h:uint16, l:uint16) = (uint32(h) <<< 16) ||| uint32(l)
let readUInt32(br:BinaryReader) =
    let h = br.ReadUInt16()
    let l = br.ReadUInt16()
    getUInt32(h, l)

let epoch = new DateTime(1970, 1, 1)
let getTime(t:uint32) = epoch.AddSeconds(float(t))

let getString(buf:byte[]) =
    let sb = new StringBuilder()
    let mutable i = 0
    while i < buf.Length && buf.[i] <> 0uy do
        sb.Append((char)buf.[i]) |> ignore
        i <- i + 1
    sb.ToString()

let pathCombine(path:string, name:string) =
    if String.IsNullOrEmpty(path) || path.EndsWith("/") then
        path + name
    else
        path + "/" + name

let isCurOrParent(path:string) = path = "." || path = ".."

let join(sep:string, objs:'a[]) =
    String.Join(sep, [| for obj in objs -> obj.ToString() |])
