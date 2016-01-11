namespace FunToolbox

open System.IO

module AtomFile = 
    open System.Text
    open System.Threading

    module private Header = 
        // we need a header, so we can discriminate between a new file and an empty one.
        let data = "atomfile\n" |> Seq.map (fun c -> c |> byte) |> Seq.toArray
        let len = data.Length
        let is (test: byte[]) = 
            test.Length = len && 
            Array.compareWith (fun (l: byte) (r: byte) -> l.CompareTo(r)) data test = 0

    [<AutoOpen>]
    module private Helper =
        let read (bytes: int) (fs: FileStream) : byte[] = 
            let a = Array.zeroCreate bytes
            if bytes <> fs.Read(a, 0, bytes) then
                failwith "failed to read from file"
            a

        let write (bytes: byte[]) (fs: FileStream) =
            fs.Write(bytes, 0, bytes.Length)

        let truncate (fs: FileStream) = 
            fs.SetLength(0 |> int64)

    /// Atomically exchanges the contents of a file.         
    let swap (f: byte[] option -> byte[] option) fn = 

        let rec openFile() = 
            let ts = 
                try
                    File.Open(fn, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)
                with 
                    // be very specific about the error we tolerate here!
                    | :? IOException as e ->
                    if e.HResult = 0x80070020 then null
                    else reraise()
            // we can not tail-recurse in exception handlers, so we do that here
            match ts with 
            | null -> 
                Thread.Sleep(1)
                openFile()
            | ts -> ts

        use fs = openFile()
        let l = fs.Length
        let li = l |> int
        let input = 
            if li = 0 then None
            else
            let header = fs |> read Header.len
            if Header.is header |> not then
                failwith "failed to read header"
            fs |> read (li - Header.len) |> Some
        
        let output = f input
        fs |> truncate
        if output.IsSome then
            try
                fs |> write Header.data
                fs |> write output.Value
            with _ ->
                // be sure file is empty when writing fails.
                fs |> truncate
                reraise()

    module Swapper = 

        /// Convert a string swap function to a byte array swap function.
        let string (e: Encoding) (f: string option -> string option) : (byte[] option -> byte[] option) = 
            Option.map (fun b -> e.GetString(b)) 
            >> f
            >> Option.map (fun str -> e.GetBytes(str))
                
                

        

        

                
   



    

