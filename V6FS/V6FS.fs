// public domain

module V6FS

open System
open System.Collections.Generic
open System.IO
open System.Text
open V6Type
open Utils

type Entry =
    { INode:int
      Name:string }
    
    static member Read(br:BinaryReader) =
        { INode = int <| br.ReadUInt16()
          Name  = getString(br.ReadBytes(14)) }

let readINode(data:byte[], ino:int, path:string, name:string) =
    let offset = 1024 + 32 * (ino - 1)
    use br = getBinaryReader(data, offset)
    { data   = data
      inode  = ino
      path   = path
      name   = name
      offset = offset
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

let readDir(x:inode) =
    let list = new List<Entry>()
    if x.IsDir then
        use br = new BinaryReader(openRead x)
        let count = x.Length / 16
        for i = 1 to count do
            let e = Entry.Read(br)
            if e.INode <> 0 && not(isCurOrParent e.Name) then
                list.Add e
        list.Sort(Comparison<Entry>(fun a b -> a.Name.CompareTo(b.Name)))
    list.ToArray()

let readFileSystem(data:byte[], offset:int) =
    use br = getBinaryReader(data, offset)
    { data   = data
      isize  = br.ReadUInt16()
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
      inodes = new Dictionary<int, inode>() }

let getINode(x:filsys, ino:int, path:string, name:string) =
    let ok, ret = x.inodes.TryGetValue(ino)
    if ok then ret else
        let ret = readINode(x.data, ino, path, name)
        x.inodes.Add(ino, ret)
        ret

let getINodes(x:filsys, ino:inode) =
    let path = ino.FullName
    let dirs = readDir ino
    [| for e in dirs ->
        getINode(x, e.INode, path, e.Name) |]

let getRoot(x:filsys) = getINode(x, 1, "", "/")

let Open(fn:string) =
    let data = File.ReadAllBytes(fn)
    let fs = readFileSystem(data, 512)
    
    use sw = new StringWriter()
    fs.Write sw
    let rec dir(inode:inode) =
        sw.WriteLine()
        inode.Write sw
        for inode in getINodes(fs, inode) do
            dir inode
    dir(getRoot fs)
    
    sw.ToString()
