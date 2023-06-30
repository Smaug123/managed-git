namespace Git.Test

open System.Threading
open FsCheck
open Git
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestString =

    [<Test>]
    let ``chopStart does nothing when chopping a non-start`` () =
        let badCount = ref 0
        let goodCount = ref 0

        let property (NonNull s : NonNull<string>) : bool =
            if s.StartsWith "hi" then
                Interlocked.Increment badCount |> ignore
                true
            else
                Interlocked.Increment goodCount |> ignore
                s |> String.chopStart "hi" |> (=) s

        Check.QuickThrowOnFailure property

        badCount.Value
        |> shouldBeSmallerThan goodCount.Value

        goodCount.Value |> shouldBeGreaterThan 10

    [<Test>]
    let ``chopStart does nothing when chopping the empty string`` () =
        let property (NonNull s) : bool = s |> String.chopStart "" |> (=) s

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``chopStart does nothing when chopping by the empty string`` () =
        let property (NonNull s) : bool = "" |> String.chopStart s |> (=) ""

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``chopStart chops the initial`` () =
        let property (NonNull toChop) (NonNull from : NonNull<string>) : bool =
            (toChop + from)
            |> String.chopStart toChop
            |> (=) from

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``chopEnd does nothing when chopping a non-end`` () =
        let badCount = ref 0
        let goodCount = ref 0

        let property (NonNull s : NonNull<string>) : bool =
            if s.EndsWith "hi" then
                Interlocked.Increment badCount |> ignore
                true
            else
                Interlocked.Increment goodCount |> ignore
                s |> String.chopEnd "hi" |> (=) s

        Check.QuickThrowOnFailure property

        badCount.Value
        |> shouldBeSmallerThan goodCount.Value

        goodCount.Value |> shouldBeGreaterThan 10

    [<Test>]
    let ``chopEnd does nothing when chopping the empty string`` () =
        let property (NonNull s) : bool = s |> String.chopEnd "" |> (=) s

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``chopEnd does nothing when chopping by the empty string`` () =
        let property (NonNull s) : bool = "" |> String.chopEnd s |> (=) ""

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``chopEnd chops the final`` () =
        let property (NonNull toChop) (NonNull from : NonNull<string>) : bool =
            (from + toChop)
            |> String.chopEnd toChop
            |> (=) from

        Check.QuickThrowOnFailure property
