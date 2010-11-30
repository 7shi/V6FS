// public domain

module V6FS

open System
open System.Collections.Generic
open System.IO
open System.Text
open V6Type
open Utils
open Crc
open Zip

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
    if (int(x.mode) &&& ILARG) = 0 then
        while i < 8 && len > 0 do
            let wlen =
                if len < 512 || i = 7 || x.addr.[i + 1] = 0us then len else 512
            array.Copy(x.data, int(x.addr.[i]) * 512, ret, pos, wlen)
            pos <- pos + wlen
            len <- len - wlen
            i <- i + 1
    else
        while i < 8 && len > 0 do
            let mutable j = 0
            let p = int(x.addr.[i]) * 512
            while j < 512 && len > 0 do
                let wlen =
                    if len < 512 || j = 510 || (x.data.[p + j + 2] = 0uy && x.data.[p + j + 3] = 0uy) then len else 512
                let b = BitConverter.ToUInt16(x.data, p + j)
                array.Copy(x.data, int(b) * 512, ret, pos, wlen)
                pos <- pos + wlen
                len <- len - wlen
                j <- j + 2
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
    
    member x.FileAttributes =
        let mutable ret = enum<FileAttributes> 0
        let mode = int <| x.INode.mode
        if (mode &&& IREAD ) =  0 then ret <- ret ||| FileAttributes.Hidden
        if (mode &&& IWRITE) =  0 then ret <- ret ||| FileAttributes.ReadOnly
        if (mode &&& IFDIR ) <> 0 then ret <- ret ||| FileAttributes.Directory
        ret

let getRoot(fsys:filsys) =
    { FileSystem = fsys
      INode = getINode(fsys, 1)
      Path  = ""
      Name  = "/"
      children = null }

let Open(fs:Stream) =
    let data = Array.zeroCreate<byte>(int fs.Length)
    fs.Read(data, 0, data.Length) |> ignore
    let fsys = readFileSystem(data, 512)
    getRoot fsys

let writeFile(list:List<ZipDirHeader>, bw:BinaryWriter, e:Entry, rel:string) =
    let len, data, crc =
        use fs = openRead e.INode
        let len = fs.Length
        let data, crc = Deflate.GetCompressBytes(fs)
        uint32 len, data, crc
    let p = uint32 bw.BaseStream.Position
    let ziph = ZipDirHeader.Create e.INode.LastWriteTime e.FileAttributes rel len crc data p
    bw.Write [| byte 'P'; byte 'K'; 3uy; 4uy |]
    ziph.header.Write bw
    bw.Write ziph.fname
    bw.Write data
    list.Add(ziph)

let rec writeDir(list:List<ZipDirHeader>, bw:BinaryWriter, e:Entry, rel:string) =
    if rel <> "" then
        let p = uint32 bw.BaseStream.Position
        let ziph = ZipDirHeader.Create e.INode.LastWriteTime e.FileAttributes (rel + "/") 0u 0u null p
        bw.Write [| byte 'P'; byte 'K'; 3uy; 4uy |]
        ziph.header.Write bw
        bw.Write ziph.fname
        list.Add(ziph)
    for e in e.Children do
        (if e.INode.IsDir then writeDir else writeFile)
            (list, bw, e, pathCombine(rel, e.Name))

let SaveZip(fs:Stream, root:Entry) =
    use bw = new BinaryWriter(fs)
    let list = new List<ZipDirHeader>()
    writeDir(list, bw, root, "")

    let dir_start = bw.BaseStream.Position
    for ziph in list do
        bw.Write [| byte 'P'; byte 'K'; 1uy; 2uy |]
        ziph.Write bw
    let dir_len = bw.BaseStream.Position - dir_start
    
    bw.Write [| byte 'P'; byte 'K'; 5uy; 6uy |]
    bw.Write 0us // number of this disk
    bw.Write 0us // number of the disk with the start of the central directory
    bw.Write (uint16 list.Count)
    bw.Write (uint16 list.Count)
    bw.Write (uint32 dir_len)
    bw.Write (uint32 dir_start)
    bw.Write 0us // zipfile comment length
