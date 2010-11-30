// public domain

module V6FS

open System
open System.Collections.Generic
open System.IO
open System.Text
open V6Type
open Utils

let empty =
    { data   = null
      offset = 0
      inode  = 0
      mode   = 0us
      nlink  = 0uy
      uid    = 0uy
      gid    = 0uy
      size0  = 0uy
      size1  = 0us
      addr   = null
      atime  = 0u
      mtime  = 0u }

let readINode(data:byte[], ino:int) =
    let offset = 1024 + 32 * (ino - 1)
    use br = getBinaryReader(data, offset)
    { data   = data
      offset = offset
      inode  = ino
      mode   = br.ReadUInt16()
      nlink  = br.ReadByte()
      uid    = br.ReadByte()
      gid    = br.ReadByte()
      size0  = br.ReadByte()
      size1  = br.ReadUInt16()
      addr   = [| for _ in 0..7 -> br.ReadUInt16() |]
      atime  = readUInt32(br)
      mtime  = readUInt32(br) }

let readAllBytes(x:inode) =
    let mutable i, pos, len = 0, 0, x.Length
    let ret = Array.zeroCreate<byte> len
    while i < 8 && len > 0 do
        let wlen =
            if i = 7 || x.addr.[i + 1] = 0us then len else 512
        array.Copy(x.data, int(x.addr.[i]) * 512, ret, pos, wlen)
        pos <- pos + wlen
        len <- len - wlen
        i <- i + 1
    ret

let openRead(x:inode) =
    new MemoryStream(readAllBytes x)

let readFileSystem(data:byte[], offset:int) =
    use br = getBinaryReader(data, offset)
    let isize = br.ReadUInt16()
    { data   = data
      offset = offset
      isize  = isize
      fsize  = br.ReadUInt16()
      nfree  = br.ReadUInt16()
      free   = [| for _ in 0..99 -> br.ReadUInt16() |]
      ninode = br.ReadUInt16()
      inode  = [| for _ in 0..99 -> br.ReadUInt16() |]
      flock  = br.ReadByte()
      ilock  = br.ReadByte()
      fmod   = br.ReadByte()
      ronly  = br.ReadByte()
      time   = readUInt32(br)
      inodes = Array.create<inode> (int(isize) * 16) empty }

let getINode(x:filsys, ino:int) =
    let ret = x.inodes.[ino - 1]
    if ret <> empty then ret else
        let ret = readINode(x.data, ino)
        x.inodes.[ino - 1] <- ret
        ret

type Entry =
    { FileSystem:filsys
      INode:inode
      Path:string
      Name:string
      mutable children:Entry[] }
    
    member private x.New(ino:int, path:string, name:string) =
        { FileSystem = x.FileSystem
          INode      = getINode(x.FileSystem, ino)
          Path       = path
          Name       = name
          children   = null }
     
    member x.FullName = pathCombine(x.Path, x.Name)
    member x.Write(tw:TextWriter) = x.INode.Write(tw, x.FullName)
    
    member x.ReadDir() =
        let list = new List<Entry>()
        if x.INode.IsDir then
            let path = x.FullName
            use br = new BinaryReader(openRead x.INode)
            let count = x.INode.Length / 16
            for i = 1 to count do
                let ino = int <| br.ReadUInt16()
                let name = getString <| br.ReadBytes(14)
                if ino <> 0 && not(isCurOrParent name) then
                    list.Add(x.New(ino, path, name))
            list.Sort(Comparison<Entry>(fun a b -> a.Name.CompareTo(b.Name)))
        x.children <- list.ToArray()
    
    member x.Children =
        if x.children = null then x.ReadDir()
        x.children

let getRoot(fsys:filsys) =
    { FileSystem = fsys
      INode = getINode(fsys, 1)
      Path  = ""
      Name  = "/"
      children = null }

let Open(fs:FileStream) =
    let data = Array.zeroCreate<byte>(int fs.Length)
    fs.Read(data, 0, data.Length) |> ignore
    let fsys = readFileSystem(data, 512)
    getRoot fsys

let GetLog(fs:FileStream) =
    use sw = new StringWriter()
#if DEBUG
    do
#else
    try
#endif
        let root = Open(fs)
        root.FileSystem.Write sw
        let rec dir(e:Entry) =
            sw.WriteLine()
            e.Write sw
            for e in e.Children do
                dir e
        dir root
#if DEBUG
#else
    with
    | e -> sw.WriteLine(e.ToString())
#endif
    sw.ToString()
