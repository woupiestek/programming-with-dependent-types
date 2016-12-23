package nl.woupiestek.andrej.java;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

@FunctionalInterface
public interface Continuation<X> {
    void apply(Consumer<X> f);

    default <Z> Continuation<Z> bind(final Function<X, Continuation<Z>> f) {
        return g -> apply(x -> f.apply(x).apply(g));
    }

    static <X> Continuation<X> unit(final X x) {
        return f -> f.accept(x);
    }

    static <X> Continuation<X> halt() {
        return f -> {
        };
    }

    default <Z> Continuation<Z> map(Function<X, Z> f) {
        return bind(x -> unit(f.apply(x)));
    }

    static <X, Y> Continuation<X> callCurrentContinuation(Function<Function<X, Continuation<Y>>, Continuation<X>> f) {
        return g -> f.apply(x -> y -> g.accept(x)).apply(g);
    }

    default Continuation<X> filter(Predicate<X> p) {
        return f -> apply(x -> {
            if (p.test(x)) f.accept(x);
        });
    }
}
