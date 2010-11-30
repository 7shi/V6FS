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

module V6Type

open System
open System.Collections.Generic
open System.IO
open Utils

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

(*
 * inode structure as it appears on
 * the disk. Not used by the system,
 * but by things like check, df, dump.
 *)

type inode =
    { data          :byte[]
      offset        :int
      inode         :int        // i number, 1-to-1 with device address
      mutable mode  :uint16
      mutable nlink :byte       // directory entries
      mutable uid   :byte       // owner
      mutable gid   :byte       // group of owner
      mutable size0 :byte       // most significant of size
      mutable size1 :uint16     // least sig
      mutable addr  :uint16[]   // device addresses constituting file
      mutable atime :uint32
      mutable mtime :uint32 }
    
    member x.Length  = (int(x.size0) <<< 16) ||| int(x.size1)
    member x.LastAccessTime = getTime x.atime
    member x.LastWriteTime  = getTime x.mtime
    member x.IsDir = (int(x.mode) &&& IFDIR ) <> 0
    
    member x.Write(tw:TextWriter, path:string) =
        tw.WriteLine("[{0:x8}] {1}", x.offset, path)
        tw.WriteLine("inode : {0}", x.inode)
        tw.WriteLine("mode  : {0}", getMode((int)x.mode))
        tw.WriteLine("nlink : {0}", x.nlink)
        tw.WriteLine("uid   : {0}", x.uid)
        tw.WriteLine("gid   : {0}", x.gid)
        tw.WriteLine("size  : {0}", x.Length)
        tw.WriteLine("addr  : {0}", join(",", x.addr))
        tw.WriteLine("atime : {0}", x.LastAccessTime)
        tw.WriteLine("mtime : {0}", x.LastWriteTime)

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
    { data          :byte[]
      offset        :int
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
      inodes        :inode[] }
    
    member x.Time = getTime x.time
    
    member x.Write(tw:TextWriter) =
        tw.WriteLine("[{0:x8}]", x.offset)
        tw.WriteLine("isize : {0}", x.isize)
        tw.WriteLine("fsize : {0}", x.fsize)
        tw.WriteLine("nfree : {0}", x.nfree)
        tw.WriteLine("ninode: {0}", x.ninode)
        tw.WriteLine("flock : {0}", x.flock)
        tw.WriteLine("ilock : {0}", x.ilock)
        tw.WriteLine("fmod  : {0}", x.fmod)
        tw.WriteLine("ronly : {0}", x.ronly)
        tw.WriteLine("time  : {0}", x.Time)
