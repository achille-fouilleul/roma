namespace Roma.Compiler

// TODO: define precise semanitcs (wrt. overflow checking)

[<Struct>]
type UInt128(hi : uint64, lo : uint64) =

    static let Mask64 = (1I <<< 64) - 1I

    static let bind1 f (n : UInt128) =
        let r : bigint = f (n.ToBigInteger())
        UInt128(r)

    static let bind2 f (a : UInt128) (b : UInt128) =
        let r : bigint = f (a.ToBigInteger()) (b.ToBigInteger())
        UInt128(r)

    static member Zero = UInt128(0UL, 0UL)

    static member One = UInt128(0UL, 1UL)

    new(n : bigint) =
        let hi = uint64(n >>> 64)
        let lo = uint64(n &&& Mask64)
        UInt128(hi, lo)

    new(n : int32) =
        UInt128(int64 n)

    new(n : uint32) =
        UInt128(uint64 n)

    new(n : uint64) =
        UInt128(0UL, n)

    new(n : int64) =
        UInt128((if n < 0L then ~~~0UL else 0UL), uint64 n)

    member private this.LowPart = lo
    member private this.HighPart = hi

    member this.ToBigInteger() =
        ((bigint hi) <<< 64) ||| (bigint lo)

    override this.ToString() =
        this.ToBigInteger().ToString()

    member this.ToString(s : string) =
        this.ToBigInteger().ToString(s)

    member this.ToString(fp : System.IFormatProvider) =
        this.ToBigInteger().ToString(fp)

    member this.ToString(s : string, fp : System.IFormatProvider) =
        this.ToBigInteger().ToString(s, fp)

    static member op_Explicit (this : UInt128) : bigint =
        this.ToBigInteger()

    static member op_Explicit (this : UInt128) : byte =
        byte this.LowPart

    static member (+) (x, y) = bind2 (+) x y

    static member (-) (x, y) = bind2 (-) x y

    static member (*) (x, y) = bind2 (*) x y

    static member (/) (x, y) = bind2 (/) x y

    static member (|||) (x, y) = bind2 (|||) x y

    static member (>>>) (x, n) = bind1 (fun x -> x >>> n) x

    static member (<<<) (x, n) = bind1 (fun x -> x <<< n) x

[<Struct>]
type Int128(hi : int64, lo : uint64)  =

    static let Mask64 = (1I <<< 64) - 1I

    static let bind1 f (n : Int128) =
        let r : bigint = f (n.ToBigInteger())
        Int128(r)

    static let bind2 f (a : Int128) (b : Int128) =
        let r : bigint = f (a.ToBigInteger()) (b.ToBigInteger())
        Int128(r)

    static member Zero = Int128(0L, 0UL)

    static member One = Int128(0L, 1UL)

    new(n : bigint) =
        let hi = int64(n >>> 64)
        let lo = uint64(n &&& Mask64)
        Int128(hi, lo)

    new(n : int32) =
        Int128(int64 n)

    new(n : uint32) =
        Int128(uint64 n)

    new(n : uint64) =
        Int128(0L, n)

    new(n : int64) =
        Int128((if n < 0L then -1L else 0L), uint64 n)

    member private this.LowPart = lo
    member private this.HighPart = hi

    member this.ToBigInteger() =
        ((bigint hi) <<< 64) ||| (bigint lo)

    override this.ToString() =
        this.ToBigInteger().ToString()

    member this.ToString(s : string) =
        this.ToBigInteger().ToString(s)

    member this.ToString(fp : System.IFormatProvider) =
        this.ToBigInteger().ToString(fp)

    member this.ToString(s : string, fp : System.IFormatProvider) =
        this.ToBigInteger().ToString(s, fp)

    static member op_Explicit (this : Int128) : byte =
        byte this.LowPart

    static member (~-) x = bind1 (~-) x

    static member (+) (x, y) = bind2 (+) x y

    static member (-) (x, y) = bind2 (-) x y

    static member (*) (x, y) = bind2 (*) x y

    static member (/) (x, y) = bind2 (/) x y

    static member (|||) (x, y) = bind2 (|||) x y

    static member (>>>) (x, n) = bind1 (fun x -> x >>> n) x

    static member (<<<) (x, n) = bind1 (fun x -> x <<< n) x

// TODO:
// - extensive tests
// - move tests to separate assembly

module Int128Tests =
    let run() =
        for i = 0 to 255 do
            let sxs = Array.create 16 (byte i)
            let sm = bigint sxs
            printfn "%s %s" (sm.ToString("d")) (sm.ToString("x"))
            let sn = Int128(sm)
            printfn "%s %s" (sn.ToString("d")) (sn.ToString("x"))
            assert(sn.ToBigInteger() = sm)

module UInt128Tests =
    let run() =
        for i = 0 to 255 do
            let uxs = [| yield! Array.create 16 (byte i); yield 0uy |]
            let um = bigint uxs
            printfn "%s %s" (um.ToString("d")) (um.ToString("x"))
            let un = UInt128(um)
            printfn "%s %s" (un.ToString("d")) (un.ToString("x"))
            assert(un.ToBigInteger() = um)