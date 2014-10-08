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

public enum Either<U, V> {

    case Right(Cell<U>)
    case Left(Cell<V>)

    public func map<W>(mapping: (U -> W)) -> Either<W, V> {
        switch self {
        case let .Right(u):
            return .Right(Cell<W>(mapping(*u)))
        case let .Left(v):
            return .Left(v)
        }
    }

    public func flatMap<W>(flatMapping: U -> Either<W, V>) -> Either<W, V> {
        switch self {
        case let .Right(u):
            return flatMapping(*u)
        case let .Left(v):
            return .Left(v)
        }
    }

    public func void() -> Either<Void, Void> {
        //TODO: void is a verb, a noun and an abjective OTL, and it sounds mutating

        switch self {
        case .Right:
            return .Right(Cell<Void>())
        case .Left:
            return .Left(Cell<Void>())
        }
    }
}

extension Either: Printable {

    public var description: String {
        switch self {
        case let .Right(u):
            return "Right{\(*u)}"
        case let .Left(v):
            return "Left{\(*v)}"
        }
    }
}


///Internal Future Callback
private enum FutureCallback<U, V> {

    typealias ResultCallback = Either<U, V> -> ()
    case Result(ResultCallback)

    typealias SuccessCallback = U -> ()
    case Success(SuccessCallback)

    typealias ErrorCallback = V -> ()
    case Error(ErrorCallback)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// MARK: Future

public class Future<U, V> {


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Private Lifecycle

    private var _result: Either<U, V>?

    private var _callbacks = Array<FutureCallback<U, V>>()

    private var _canceled = false

    private init() {}

    private func succeed(u: U) {

        complete(Either.Right(Cell<U>(u)))
    }

    private func fail(v: V) {

        complete(Either.Left(Cell<V>(v)))
    }

    private func complete(res: Either<U, V>) {

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

    private func call(callback: FutureCallback<U, V>) {

        if _canceled {
            return
        }

        if let res = _result {

            switch callback {
            case let .Result(f):
                f(res)
            case let .Success(f):
                switch res {
                case let .Right(r):
                    f(*r)
                case .Left:
                    break
                }
            case let .Error(f):
                switch res {
                case .Right:
                    break
                case let .Left(l):
                    f(*l)
                }
            }
        }
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Accessor

    public var result: Either<U, V>? {

        return _result
    }

    public var completed: Bool {

        return result != nil
    }

    public var success: U? {

        if !completed {
            return nil
        }
        switch result! {
        case let Either.Right(u):
            return *u
        default:
            return nil
        }
    }

    public var succeeded: Bool {

        return success != nil
    }

    public var error: V? {

        if !completed {
            return nil
        }
        switch result! {
        case let Either.Left(v):
            return *v
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

    public func onResult(resultCallback: Either<U, V> -> ()) -> Future<U, V> {

        let futureCallback = FutureCallback.Result(resultCallback)
        if _result == nil {
            _callbacks.append(futureCallback)
        }
        else {
            call(futureCallback)
        }
        return self
    }

    public func onSuccess(successCallback: U -> ()) -> Future<U, V> {

        let futureCallback = FutureCallback<U, V>.Success(successCallback)
        if _result == nil {
            _callbacks.append(futureCallback)
        }
        else {
            call(futureCallback)
        }
        return self
    }

    public func onError(errorCallback: V -> ()) -> Future<U, V> {

        let futureCallback = FutureCallback<U, V>.Error(errorCallback)
        if _result == nil {
            _callbacks.append(futureCallback)
        }
        else {
            call(futureCallback)
        }
        return self
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Transform

    public func map <W> (mapping: U -> W) -> Future<W, V> {

        return mapResult() {
            e in
            e.map(mapping)
        }
    }

    public func mapResult <W, X> (mapping: Either<U, V> ->  Either<W, X>) -> Future<W, X> {

        var p = Promise<W, X>()
        onResult() {
            e in
            p.complete(mapping(e))
        }
        return p._future
    }

    public func chain <W> (mapping: U -> Future<W, V>) -> Future<W, V> {

        func chaining(e: Either<U, V>) -> Future<W, V> {

            switch e {
            case let .Right(u):

                return mapping(*u)
            case let .Left(v):
                return Future<W, V>.Failed(*v)
            }
        }

        return chainResult(chaining)
    }

    public func chainResult <W> (mapping: Either<U, V> -> Future<W, V>) -> Future<W, V> {

        var p = Promise<W, V>()

        onResult() {
            e in
            mapping(e).onResult() {
                futResult in
                p.complete(futResult)
            }
            return
        }
        return p._future
    }

    public func void() -> Future<Void, Void> {

        return mapResult() {
            e in
            e.void()
        }
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Util - Constructors

    public class func Completed<U, V>(e : Either<U, V>) -> Future<U, V> {

        var f = Future<U, V>()
        f.complete(e)
        return f
    }

    public class func Succeeded<U, V>(u : U) -> Future<U, V> {

        var f = Future<U, V>()
        f.succeed(u)
        return f
    }

    public class func Failed<U, V>(v : V) -> Future<U, V> {

        var f = Future<U, V>()
        f.fail(v)
        return f
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Util - Aggregations
    //TODO: decide if it belongs in Future or Promise

    public class func AllResult(futures: Array<Future<U, V>>) -> Future<Array<Either<U, V>>, Void> {

        assert(futures.count > 0)

        var p = Promise<Array<Either<U, V>>, Void>()
        var resultCount = 0

        for future in futures {
            future.onResult() {
                e in
                resultCount += 1
                if resultCount == futures.count {

                    p.succeed(futures.map({ $0.result! }))
                }
            }
        }
        return p.future
    }

    public class func AllSuccess(futures: Array<Future<U, V>>) -> Future<Array<U>, V> {

        assert(futures.count > 0)

        var p = Promise<Array<U>, V>()
        var successCount = 0

        for future in futures {
            future.onSuccess() {
                u in
                successCount += 1
                if successCount == futures.count {

                    p.succeed(futures.map({ $0.success! }))
                }
            }
            future.onError() {
                v in
                p.fail(v)
            }
        }
        return p.future
    }

    public class func Any(futures: Array<Future<U, V>>) -> Future<U, V> {

        assert(futures.count > 0)

        var p = Promise<U, V>()
        var successCount = 0

        for future in futures {
            future.onSuccess() {
                u in
                p.succeed(u)
            }
            future.onError() {
                v in
                p.fail(v)
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

public class Promise<U, V> {


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Future Accessor

    private var _future = Future<U, V>()

    var future: Future<U, V> {

        return _future
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // MARK: Lifecycle: mutating the underlying future

    public func complete(res: Either<U, V>) {

        _future.complete(res)
    }

    public func succeed(u: U) {
        
        _future.succeed(u)
    }
    
    public func fail(v: V) {
        
        _future.fail(v)
    }
    
    public func cancel() {
        
        _future.cancel()
    }
    
    public func follow(otherFuture: Future<U, V>) {
        //TODO: find a good name
        
        otherFuture.onResult(complete)
    }
    
    public func unless<W, X>(otherFuture: Future<W, X>, transform: W -> V) {
        
        otherFuture.onSuccess() {
            w in
            self.fail(transform(w))
        }
    }
}

extension Promise: Printable {
    
    public var description: String {
        return "Promise{\(_future)}"
    }
}

