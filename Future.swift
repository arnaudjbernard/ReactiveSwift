////////////////////////////////////////////////////////////////////////////////////////////////////
//  Created by ArnaudJBernard.
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// MARK: Dirty trick to keep the compiler happy

public class Cell<A> {
    private let val: A

    init(_ val: A) {

        self.val = val
    }
}

prefix operator * {}
public prefix func * <A> (c: Cell<A>) -> A {
    return c.val
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// MARK: Either

public enum Either<L, R> {

    case Left(Cell<L>)
    case Right(Cell<R>)

    public func map<NR>(mapping: (R -> NR)) -> Either<L, NR> {
        switch self {
        case let .Left(l):
            return .Left(l)
        case let .Right(r):
            return .Right(Cell<NR>(mapping(*r)))
        }
    }

    public func flatMap<NR>(flatMapping: R -> Either<L, NR>) -> Either<L, NR> {
        switch self {
        case let .Left(l):
            return .Left(l)
        case let .Right(r):
            return flatMapping(*r)
        }
    }

    public func void() -> Either<Void, Void> {
        //TODO: void is a verb, a noun and an abjective OTL, and it sounds mutating

        switch self {
        case .Left:
            return .Left(Cell<Void>())
        case .Right:
            return .Right(Cell<Void>())
        }
    }
}

extension Either: Printable {

    public var description: String {
        switch self {
        case let .Left(l):
            return "Left{\(*l)}"
        case let .Right(r):
            return "Right{\(*r)}"
        }
    }
}


///Internal Future Callback
private enum FutureCallback<E, S> {

    typealias ResultCallback = Either<E, S> -> ()
    case Result(ResultCallback)

    typealias SuccessCallback = S -> ()
    case Success(SuccessCallback)

    typealias ErrorCallback = E -> ()
    case Error(ErrorCallback)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// MARK: Future

public class Future<E, S> {


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Private Lifecycle

    private var _result: Either<E, S>?

    private var _callbacks = Array<FutureCallback<E, S>>()

    private var _canceled = false

    private init() {}

    private func succeed(s: S) {

        complete(Either.Right(Cell<S>(s)))
    }

    private func fail(e: E) {

        complete(Either.Left(Cell<E>(e)))
    }

    private func complete(res: Either<E, S>) {

        if _result != nil {
            return
        }

        _result = res

        for callback in _callbacks {

            call(callback)
        }
        _callbacks.removeAll()
    }

    private func cancel() {
        //TODO: this may not be a good idea, it breaks the contract

        _canceled = true
        _callbacks.removeAll()
    }

    private func call(callback: FutureCallback<E, S>) {

        if _canceled {
            return
        }

        if let res = _result {

            switch callback {
            case let .Result(f):
                f(res)
            case let .Success(f):
                switch res {
                case .Left:
                    break
                case let .Right(s):
                    f(*s)
                }
            case let .Error(f):
                switch res {
                case let .Left(e):
                    f(*e)
                case .Right:
                    break
                }
            }
        }
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Accessor

    public var result: Either<E, S>? {

        return _result
    }

    public var completed: Bool {

        return result != nil
    }

    public var success: S? {

        if !completed {
            return nil
        }
        switch result! {
        case let Either.Right(s):
            return *s
        default:
            return nil
        }
    }

    public var succeeded: Bool {

        return success != nil
    }

    public var error: E? {

        if !completed {
            return nil
        }
        switch result! {
        case let Either.Left(e):
            return *e
        default:
            return nil
        }
    }

    public var failed: Bool {

        return error != nil
    }

    public var canceled: Bool {

        return _canceled
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: In place

    public func onResult(resultCallback: Either<E, S> -> ()) -> Future<E, S> {

        let futureCallback = FutureCallback.Result(resultCallback)
        if completed {
            call(futureCallback)
        }
        else {
            _callbacks.append(futureCallback)
        }
        return self
    }

    public func onSuccess(successCallback: S -> ()) -> Future<E, S> {

        let futureCallback = FutureCallback<E, S>.Success(successCallback)
        if completed {
            call(futureCallback)
        }
        else {
            _callbacks.append(futureCallback)
        }
        return self
    }

    public func onError(errorCallback: E -> ()) -> Future<E, S> {

        let futureCallback = FutureCallback<E, S>.Error(errorCallback)
        if completed {
            call(futureCallback)
        }
        else {
            _callbacks.append(futureCallback)
        }
        return self
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Transform

    public func map <NS> (mapping: S -> NS) -> Future<E, NS> {

        return mapResult() {
            either in
            either.map(mapping)
        }
    }

    public func mapResult <NE, NS> (mapping: Either<E, S> -> Either<NE, NS>) -> Future<NE, NS> {

        var p = Promise<NE, NS>()
        onResult() {
            either in
            p.complete(mapping(either))
        }
        return p.future
    }

    public func chain <NS> (mapping: S -> Future<E, NS>) -> Future<E, NS> {

        func chaining(either: Either<E, S>) -> Future<E, NS> {

            switch either {
            case let .Right(s):
                return mapping(*s)
            case let .Left(e):
                return Future<E, NS>.Failed(*e)
            }
        }

        return chainResult(chaining)
    }

    public func chainResult <NS> (mapping: Either<E, S> -> Future<E, NS>) -> Future<E, NS> {

        var p = Promise<E, NS>()

        onResult() {
            either in
            mapping(either).onResult() {
                futResult in
                p.complete(futResult)
            }
            return
        }
        return p.future
    }

    public func void() -> Future<Void, Void> {

        return mapResult() {
            either in
            either.void()
        }
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Util - Constructors

    public class func Completed<E, S>(either : Either<E, S>) -> Future<E, S> {

        var f = Future<E, S>()
        f.complete(either)
        return f
    }

    public class func Succeeded<E, S>(s : S) -> Future<E, S> {

        var f = Future<E, S>()
        f.succeed(s)
        return f
    }

    public class func Failed<E, S>(e : E) -> Future<E, S> {

        var f = Future<E, S>()
        f.fail(e)
        return f
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Util - Aggregations
    //TODO: decide if it belongs in Future or Promise

    public class func AllResult(futures: Array<Future<E, S>>) -> Future<Void, Array<Either<E, S>>> {

        assert(futures.count > 0)

        var p = Promise<Void, Array<Either<E, S>>>()
        var resultCount = 0

        for future in futures {
            future.onResult() {
                _ in
                resultCount += 1
                if resultCount == futures.count {
                    p.succeed(futures.map({ $0.result! }))
                }
            }
        }
        return p.future
    }

    public class func AllSuccess(futures: Array<Future<E, S>>) -> Future<E, Array<S>> {

        assert(futures.count > 0)

        var p = Promise<E, Array<S>>()
        var successCount = 0

        for future in futures {
            future.onSuccess() {
                _ in
                successCount += 1
                if successCount == futures.count {

                    p.succeed(futures.map({ $0.success! }))
                }
            }
            future.onError() {
                e in
                p.fail(e)
            }
        }
        return p.future
    }

    public class func Any(futures: Array<Future<E, S>>) -> Future<E, S> {

        assert(futures.count > 0)

        var p = Promise<E, S>()
        var successCount = 0

        for future in futures {
            future.onSuccess() {
                s in
                p.succeed(s)
            }
            future.onError() {
                e in
                p.fail(e)
            }
        }
        return p.future
    }
}

extension Future: Printable {

    public var description: String {

        return "Future{\(result)}"
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// MARK: Promise

public class Promise<E, S> {


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Future Accessor

    private var _future = Future<E, S>()

    var future: Future<E, S> {

        return _future
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Lifecycle: mutating the underlying future

    public func complete(res: Either<E, S>) {

        _future.complete(res)
    }

    public func succeed(s: S) {

        _future.succeed(s)
    }

    public func fail(e: E) {

        _future.fail(e)
    }

    public func cancel() {

        _future.cancel()
    }

    public func follow(otherFuture: Future<E, S>) {
        //TODO: find a good name

        otherFuture.onResult(complete)
    }

    public func unless<_E, NS>(otherFuture: Future<_E, NS>, errorBuilder: NS -> E) {

        otherFuture.onSuccess() {
            ns in
            self.fail(errorBuilder(ns))
        }
    }
}

extension Promise: Printable {

    public var description: String {
        return "Promise{\(_future)}"
    }
}

