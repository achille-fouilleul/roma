namespace Roma.Cli

open System
open System.IO
open System.Security.Cryptography
open LittleEndian

type PEVirtSlice =
    {
        rva : uint32
        size : uint32
    }

    member this.IsZero =
        this = { rva = 0u; size = 0u }

type CliHeader =
    {
        runtimeVersion : uint16 * uint16
        metaData : PEVirtSlice
        entryPointToken : uint32
        strongNameSig : PEVirtSlice
    }

type PEImageReader(path : string) =
    let data = File.ReadAllBytes(path)

    let read16 pos =
        u16 data pos

    let read32 pos =
        u32 data pos

    let readRvaSize pos =
        {
            rva = read32 pos
            size = read32 (pos + 4)
        }

    let readArr pos size =
        Array.sub data pos size

    let lfanew = read32 0x3c |> int
    do if readArr lfanew 4 <> "PE\x00\x00"B then
        failwith "Invalid PE signature."
    let peHdrOff = lfanew + 4
    let peOptHdrSize = 28 + 68 + 128
    do if read16 (peHdrOff + 16) |> int <> peOptHdrSize then
        failwith "Invalid optional header size."
    let nSect = read16 (peHdrOff + 2) |> int
    let peOptHdrOff = peHdrOff + 20
    let sectHdrOff = peOptHdrOff + peOptHdrSize
    let zeroSpans =
        [
            peOptHdrOff + 64, 4
            peOptHdrOff + 128, 8
        ]
    let sectTable =
        [
            for i in 0 .. (nSect - 1) ->
                let off = sectHdrOff + 40 * i
                let s = read32 (off + 12)
                let l = read32 (off + 16)
                let p = read32 (off + 20)
                s, l, p
        ]
    let sectTableEnd = sectHdrOff + 40 * nSect
    let rvaToOff r =
        let s, l, p = List.find (fun (s, l, p) -> s <= r && r < s + l) sectTable
        p + r - s |> int
    let cliHdr = readRvaSize (peOptHdrOff + 208)

    member this.Item
        with get(rva) = data.[rvaToOff(uint32 rva)]

    member this.Read(rva, size) =
        if rva = 0 then
            raise(ArgumentNullException "rva")
        readArr (rvaToOff(uint32 rva)) size

    member this.Read(slice : PEVirtSlice) =
        if slice.IsZero then
            raise(ArgumentNullException "slice")
        readArr (rvaToOff slice.rva) (int slice.size)

    member this.CliHeader =
        if cliHdr.IsZero then
            None
        else
            let off = rvaToOff cliHdr.rva
            Some {
                runtimeVersion = read16(off + 4), read16(off + 6)
                metaData = readRvaSize(off + 8)
                entryPointToken = read32(off + 20)
                strongNameSig = readRvaSize(off + 32)
            }

    member this.StrongNameHash =
        use sha1 = new SHA1Managed()

        let hash block =
            sha1.TransformBlock(block, 0, block.Length, block, 0) |> ignore

        do
            let block = readArr 0 sectTableEnd
            for (off, n) in zeroSpans do
                for i = off to (off + n - 1) do
                    block.[i] <- 0uy
            hash block

        let sigLoc =
            this.CliHeader
            |> Option.bind begin fun cliHeader ->
                match cliHeader.strongNameSig with
                | loc when not(loc.IsZero) -> Some(rvaToOff loc.rva, loc.size)
                | _ -> None
            end

        for _, l, p in sectTable do
            match sigLoc with
            | Some (off, size) when uint32 off >= p && uint32 off + size <= p + l ->
                readArr (int p) (off - (int p)) |> hash
                readArr (off + int size) (int (p + l) - (off + int size)) |> hash
            | _ ->
                readArr (int p) (int l) |> hash

        sha1.TransformFinalBlock(Array.empty, 0, 0) |> ignore
        sha1.Hash
