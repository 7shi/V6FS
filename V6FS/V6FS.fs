(*
 * Copyright(C) Caldera International Inc. 2001-2002. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * Redistributions of source code and documentation must retain the above copyright
 * notice, this list of conditions and the following disclaimer. Redistributions
 * in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * All advertising materials mentioning features or use of this software mus
 * display the following acknowledgement:
 *
 * This product includes software developed or owned by Caldera International, Inc.
 *
 * Neither the name of Caldera International, Inc. nor the names of other
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.

 * USE OF THE SOFTWARE PROVIDED FOR UNDER THIS LICENSE BY CALDERA INTERNATIONAL, INC.
 * AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL CALDERA INTERNATIONAL, INC. BE LIABLE FOR
 * ANY DIRECT, INDIRECT INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

module V6FS

open System
open System.Collections.Generic
open System.IO
open System.Text

let NINODE = 100
let ROOTINO = 1
let ILOCK  = 1uy

let IALLOC  = 0o100000
let IFMT    = 0o60000
let IFDIR   = 0o40000
let IFCHR   = 0o20000
let IFBLK   = 0o60000
let ILARG   = 0o10000
let ISUID   = 0o4000
let ISGID   = 0o2000
let ISVTX   = 0o1000
let IREAD   = 0o400
let IWRITE  = 0o200
let IEXEC   = 0o100

let right(s:string, len:int) = s.Substring(s.Length - len)

let getMode(mode:int) =
    let list = new List<string>()
    list.Add(right("00" + Convert.ToString(mode &&& 0o777, 8), 3))
    if (mode &&& IALLOC) <> 0 then list.Add "IALLOC"
    if (mode &&& IFMT  ) <> 0 then list.Add "IFMT"
    if (mode &&& IFDIR ) <> 0 then list.Add "IFDIR"
    if (mode &&& IFCHR ) <> 0 then list.Add "IFCHR"
    if (mode &&& IFBLK ) <> 0 then list.Add "IFBLK"
    if (mode &&& ILARG ) <> 0 then list.Add "ILARG"
    if (mode &&& ISUID ) <> 0 then list.Add "ISUID"
    if (mode &&& ISGID ) <> 0 then list.Add "ISGID"
    if (mode &&& ISVTX ) <> 0 then list.Add "ISVTX"
//    if (mode &&& IREAD ) <> 0 then list.Add "IREAD"
//    if (mode &&& IWRITE) <> 0 then list.Add "IWRITE"
//    if (mode &&& IEXEC ) <> 0 then list.Add "IEXEC"
    String.Join(",", list.ToArray())

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

type dentry =
    { inode:int
      name:string }
    
    static member Read(br:BinaryReader) =
        { inode = int <| br.ReadUInt16()
          name  = getString(br.ReadBytes(14)) }

(*
 * inode structure as it appears on
 * the disk. Not used by the system,
 * but by things like check, df, dump.
 *)

type inode =
    { data:byte[]
      inode:int             // i number, 1-to-1 with device address
      path:string
      name:string
      offset:int
      mutable mode:uint16
      mutable nlink:byte    // directory entries
      mutable uid:byte      // owner
      mutable gid:byte      // group of owner
      mutable size0:byte    // most significant of size
      mutable size1:uint16  // least sig
      mutable addr:uint16[] // device addresses constituting file
      mutable atime:uint32
      mutable mtime:uint32 }
    
    static member Read(data:byte[], ino:int, path:string, name:string) =
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
    
    member x.GetAddrs() =
        String.Join(",", [| for ad in x.addr -> ad.ToString() |])
    
    member x.Length  = (int(x.size0) <<< 16) ||| int(x.size1)
    member x.LastAccessTime = getTime x.atime
    member x.LastWriteTime  = getTime x.mtime
    member x.FullName = pathCombine(x.path, x.name)
    member x.IsDir = (int(x.mode) &&& IFDIR ) <> 0
    
    member x.Write(tw:TextWriter) =
        tw.WriteLine("[{0}]", x.FullName)
        tw.WriteLine("mode  : {0}", getMode((int)x.mode))
        tw.WriteLine("nlink : {0}", x.nlink)
        tw.WriteLine("uid   : {0}", x.uid)
        tw.WriteLine("gid   : {0}", x.gid)
        tw.WriteLine("size  : {0}", x.Length)
        tw.WriteLine("addr  : {0}", x.GetAddrs())
        tw.WriteLine("atime : {0}", x.LastAccessTime)
        tw.WriteLine("mtime : {0}", x.LastWriteTime)
    
    member x.ReadAllBytes() =
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
    
    member x.OpenRead() =
        new MemoryStream(x.ReadAllBytes())
    
    member x.ReadDir() =
        let list = new List<dentry>()
        if x.IsDir then
            use br = new BinaryReader(x.OpenRead())
            let count = x.Length / 16
            for i = 1 to count do
                let dent = dentry.Read(br)
                if dent.inode <> 0 && not(isCurOrParent dent.name) then
                    list.Add dent
            list.Sort(new Comparison<dentry>(fun a b -> a.name.CompareTo(b.name)))
        list.ToArray()

(*
 * Definition of the unix super block.
 * The root super block is allocated and
 * read in iinit/alloc.c. Subsequently
 * a super block is allocated and read
 * with each mount (smount/sys3.c) and
 * released with unmount (sumount/sys3.c).
 * A disk block is ripped off for storage.
 * See alloc.c for general alloc/free
 * routines for free list and I list.
 *)

type filsys =
    { data:byte[]
      mutable isize :uint16   // size in blocks of I list
      mutable fsize :uint16   // size in blocks of entire volume
      mutable nfree :uint16   // number of in core free blocks (0-100)
      free          :uint16[] // in core free blocks
      mutable ninode:uint16   // number of in core I nodes (0-100)
      inode         :uint16[] // in core free I nodes
      mutable flock :byte     // lock during free list manipulation
      mutable ilock :byte     // lock during I list manipulation
      mutable fmod  :byte     // super block modified flag
      mutable ronly :byte     // mounted read-only flag
      time          :uint32   // current date of last update
      inodes        :Dictionary<int, inode> }
    
    static member Read(data:byte[], offset:int) =
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
    
    member x.Time = getTime x.time
    
    member x.Write(tw:TextWriter) =
        tw.WriteLine("isize : {0}", x.isize)
        tw.WriteLine("fsize : {0}", x.fsize)
        tw.WriteLine("nfree : {0}", x.nfree)
        tw.WriteLine("ninode: {0}", x.ninode)
        tw.WriteLine("flock : {0}", x.flock)
        tw.WriteLine("ilock : {0}", x.ilock)
        tw.WriteLine("fmod  : {0}", x.fmod)
        tw.WriteLine("ronly : {0}", x.ronly)
        tw.WriteLine("time  : {0}", x.Time)
    
    member x.GetINode(ino:int, path:string, name:string) =
        let ok, ret = x.inodes.TryGetValue(ino)
        if ok then ret else
            let ret = inode.Read(x.data, ino, path, name)
            x.inodes.Add(ino, ret)
            ret
    
    member x.GetNodes(ino:inode) =
        let path = ino.FullName
        let dirs = ino.ReadDir()
        [| for dent in dirs ->
            x.GetINode(dent.inode, path, dent.name) |]
    
    member x.Root = x.GetINode(1, "", "/")

let Open(fn:string) =
    let data = File.ReadAllBytes(fn)
    let fs = filsys.Read(data, 512)
    
    use sw = new StringWriter()
    fs.Write sw
    let rec dir(inode:inode) =
        sw.WriteLine()
        inode.Write sw
        for inode in fs.GetNodes(inode) do
            dir inode
    dir fs.Root
    
    sw.ToString()
